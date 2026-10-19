
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <random>
#include <cmath>
#include <algorithm>
#include <chrono>
#include <cstdlib>

using namespace std;


// Structure d'une opération : machine et durée
struct Operation {
    int machine;
    int duree;
};


// Structure d'une opération planifiée
struct OperationPlanifiee {
    int machine;
    int start;
    int end;
};


// Structure de l'instance JSP
struct Instance {
    vector<vector<Operation>> jobs;
    int nb_machines;
};


// Structure du résultat
struct Resultat {
    int makespan;
    vector<vector<OperationPlanifiee>> planning;
};


// --- Lecture de l'instance ---

Instance lire_instance(string nom_fichier) {

    ifstream fichier(nom_fichier);

    if (!fichier) {
        cerr << "Impossible d'ouvrir : " << nom_fichier << endl;
        exit(1);
    }

    string ligne;
    getline(fichier, ligne);  // Nom de l'instance

    int nb_jobs, nb_machines;
    fichier >> nb_jobs >> nb_machines;

    getline(fichier, ligne);  // Fin de la deuxième ligne

    Instance instance;
    instance.nb_machines = nb_machines;

    for (int j = 0; j < nb_jobs; j++) {

        getline(fichier, ligne);
        istringstream lecture(ligne);

        vector<Operation> operations;
        int machine, duree;

        // Lire les nombres deux par deux : machine, durée
        while (lecture >> machine >> duree) {
            operations.push_back({ machine, duree });
        }

        instance.jobs.push_back(operations);
    }

    return instance;
}


// --- Construction d'un planning ---

Resultat construire_planning(
    const vector<vector<Operation>>& jobs,
    int nb_machines,
    double beta,
    mt19937& generateur
) {

    int nb_jobs = (int)jobs.size();

    vector<int> fin_machines(nb_machines, 0);
    vector<int> fin_jobs(nb_jobs, 0);
    vector<int> prochaine_operation(nb_jobs, 0);

    vector<vector<OperationPlanifiee>> planning(nb_jobs);

    int nb_operations = 0;

    for (int j = 0; j < nb_jobs; j++) {
        nb_operations += (int)jobs[j].size();
    }

    for (int n = 0; n < nb_operations; n++) {

        vector<int> jobs_disponibles;
        vector<double> poids;

        // Calculer un poids pour chaque job disponible
        for (int j = 0; j < nb_jobs; j++) {

            if (prochaine_operation[j] < jobs[j].size()) {

                int op = prochaine_operation[j];

                int machine = jobs[j][op].machine;
                int duree = jobs[j][op].duree;

                int debut_possible = max(
                    fin_jobs[j],
                    fin_machines[machine]
                );

                int fin_possible = debut_possible + duree;

                // Heuristique B : favoriser les fins rapides
                double heuristique = 1.0 / (1 + fin_possible);
                double poids_job = pow(heuristique, beta);

                poids.push_back(poids_job);
                jobs_disponibles.push_back(j);
            }
        }

        // Choisir un job au hasard selon les poids
        discrete_distribution<int> distribution(
            poids.begin(), poids.end()
        );

        int indice = distribution(generateur);
        int j = jobs_disponibles[indice];

        // Programmer la prochaine opération du job
        int op = prochaine_operation[j];

        int machine = jobs[j][op].machine;
        int duree = jobs[j][op].duree;

        int debut = max(fin_jobs[j], fin_machines[machine]);
        int fin = debut + duree;

        // Enregistrer l'opération
        planning[j].push_back({ machine, debut, fin });

        // Mettre à jour les disponibilités
        fin_jobs[j] = fin;
        fin_machines[machine] = fin;

        // Passer à l'opération suivante de ce job
        prochaine_operation[j]++;
    }

    Resultat resultat;

    resultat.makespan = *max_element(
        fin_jobs.begin(), fin_jobs.end()
    );

    resultat.planning = planning;

    return resultat;
}


// --- Vérification indépendante d'un planning ---

bool verifier_planning(
    const Instance& instance,
    const Resultat& resultat,
    string& erreur
) {
    erreur.clear();

    if (instance.nb_machines <= 0 || instance.jobs.empty()) {
        erreur = "L'instance doit contenir des jobs et des machines.";
        return false;
    }

    if (resultat.planning.size() != instance.jobs.size()) {
        erreur = "Le nombre de jobs du planning est incorrect.";
        return false;
    }

    vector<vector<OperationPlanifiee>> operations_par_machine(instance.nb_machines);
    int fin_maximale = 0;

    for (size_t j = 0; j < instance.jobs.size(); j++) {
        const auto& operations = instance.jobs[j];
        const auto& planning_job = resultat.planning[j];

        if (operations.empty() || planning_job.size() != operations.size()) {
            erreur = "Nombre d'operations incorrect pour le job " + to_string(j) + ".";
            return false;
        }

        int fin_precedente = 0;

        for (size_t i = 0; i < operations.size(); i++) {
            const auto& attendue = operations[i];
            const auto& planifiee = planning_job[i];
            string position = "Job " + to_string(j) + ", operation " + to_string(i) + " : ";

            if (attendue.machine < 0 || attendue.machine >= instance.nb_machines ||
                attendue.duree <= 0) {
                erreur = position + "donnees de l'instance invalides.";
                return false;
            }

            if (planifiee.machine != attendue.machine) {
                erreur = position + "machine incorrecte.";
                return false;
            }

            if (planifiee.start < 0 || planifiee.end < planifiee.start) {
                erreur = position + "dates invalides.";
                return false;
            }

            // Le calcul en long long évite un débordement dans le vérificateur.
            if (static_cast<long long>(planifiee.end) - planifiee.start != attendue.duree) {
                erreur = position + "duree incorrecte.";
                return false;
            }

            if (planifiee.start < fin_precedente) {
                erreur = position + "ordre du job non respecte.";
                return false;
            }

            fin_precedente = planifiee.end;
            fin_maximale = max(fin_maximale, planifiee.end);
            operations_par_machine[planifiee.machine].push_back(planifiee);
        }
    }

    for (int machine = 0; machine < instance.nb_machines; machine++) {
        auto& operations = operations_par_machine[machine];

        // Cette petite fonction indique à sort comment comparer deux opérations.
        sort(operations.begin(), operations.end(),
            [](const OperationPlanifiee& a, const OperationPlanifiee& b) {
                return a.start < b.start;
            });

        for (size_t i = 1; i < operations.size(); i++) {
            if (operations[i].start < operations[i - 1].end) {
                erreur = "Chevauchement sur la machine " + to_string(machine) + ".";
                return false;
            }
        }
    }

    if (resultat.makespan != fin_maximale) {
        erreur = "Makespan incorrect : annonce " + to_string(resultat.makespan) +
            ", attendu " + to_string(fin_maximale) + ".";
        return false;
    }

    return true;
}


// --- Enregistrement au format JSON ---

void enregistrer_json(string nom_fichier, const Resultat& resultat) {

    ofstream fichier(nom_fichier);

    if (!fichier) {
        cerr << "Impossible de creer : " << nom_fichier << endl;
        exit(1);
    }

    fichier << "{\n";
    fichier << "    \"makespan\": " << resultat.makespan << ",\n";
    fichier << "    \"jobs_schedule\": {\n";

    for (int j = 0; j < resultat.planning.size(); j++) {

        fichier << "        \"Job_" << j << "\": [\n";

        const auto& operations = resultat.planning[j];

        for (int i = 0; i < operations.size(); i++) {

            const auto& op = operations[i];

            fichier << "            {\"machine\": " << op.machine
                << ", \"start\": " << op.start
                << ", \"end\": " << op.end << "}";

            if (i + 1 < operations.size()) {
                fichier << ",";
            }

            fichier << "\n";
        }

        fichier << "        ]";

        if (j + 1 < resultat.planning.size()) {
            fichier << ",";
        }

        fichier << "\n";
    }

    fichier << "    }\n";
    fichier << "}\n";
}


// --- Programme principal ---

// Les tests réutilisent les fonctions ci-dessus avec leur propre main.
#ifndef JSP_SANS_MAIN
int main() {

    // Modifier ces valeurs, puis cliquer sur le bouton vert de Visual Studio.
    string nom = "6_6";            // Instance : "3_3", "6_6", "10_5", "20_10", "20_15"
    int nb_essais = 100000;        // Nombre de plannings à construire
    double beta = 2.0;             // Influence de l'heuristique B
    unsigned int graine = 12345;   // Changer ce nombre pour faire un autre test

    if (nb_essais <= 0) {
        cerr << "Le nombre d'essais doit etre strictement positif." << endl;
        return 1;
    }

    // Une même graine reproduit les tirages avec le même exécutable et les mêmes paramètres.
    mt19937 generateur(graine);

    cout << "Instance : " << nom << ", essais : " << nb_essais
        << ", beta : " << beta << ", graine : " << graine << endl;

    // Lecture du fichier
    Instance instance = lire_instance("Base/" + nom + ".txt");

    Resultat meilleur_resultat;
    meilleur_resultat.makespan = -1;

    // Début du chronomètre
    auto debut = chrono::steady_clock::now();

    // Générer plusieurs plannings
    for (int essai = 0; essai < nb_essais; essai++) {

        Resultat resultat = construire_planning(
            instance.jobs,
            instance.nb_machines,
            beta,
            generateur
        );

        // Conserver le meilleur planning
        if (meilleur_resultat.makespan == -1 ||
            resultat.makespan < meilleur_resultat.makespan) {

            meilleur_resultat = resultat;

            cout << "Essai " << essai + 1
                << " : nouveau meilleur makespan = "
                << resultat.makespan << endl;
        }
    }

    // Fin du chronomètre
    auto fin = chrono::steady_clock::now();

    double temps = chrono::duration<double>(fin - debut).count();

    // Vérifier le meilleur résultat avant de créer ou de remplacer le JSON.
    string erreur;

    if (!verifier_planning(instance, meilleur_resultat, erreur)) {
        cerr << "Planning invalide : " << erreur << endl;
        return 1;
    }

    cout << "Planning valide." << endl;

    // Export JSON
    enregistrer_json(
        "Base/" + nom + ".json",
        meilleur_resultat
    );

    cout << "JSON enregistre : Base/" << nom << ".json" << endl;

    cout << "\nMeilleur makespan apres " << nb_essais
        << " essais : " << meilleur_resultat.makespan << endl;

    cout << "Temps d'execution : "
        << temps << " secondes" << endl;

    return 0;
}
#endif
