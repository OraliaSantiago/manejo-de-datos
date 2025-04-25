#include <iostream> // Biblioteca para entrada y salida est�ndar
#include <vector>   // Biblioteca para manejar arreglos din�micos (vectores)

using namespace std;
// Declaraci�n de la funci�n merge antes de su uso
void merge(vector<int>& arr, int izquierda, int pmedio, int derecha);


// Funci�n recursiva para dividir y ordenar el arreglo usando Merge Sort
void mergeSort(vector<int>& arr, int izquierda, int derecha) {
    if (izquierda < derecha) { // Si hay m�s de un elemento en la secci�n a ordenar
        int pmedio = izquierda + (derecha - izquierda) / 2; // Calcula el punto medio

        // Llamadas recursivas para ordenar cada mitad del arreglo
        mergeSort(arr, izquierda, pmedio);       // Ordena la mitad izquierda
        mergeSort(arr, pmedio + 1, derecha);     // Ordena la mitad derecha

        // Fusiona las dos mitades ordenadas en el arreglo original
        merge(arr, izquierda, pmedio, derecha);
    }
}

// Funci�n para fusionar dos subarreglos ordenados en el arreglo original
void merge(vector<int>& arr, int izquierda, int pmedio, int derecha) {
    // Calcular los tama�os de los subarreglos
    int n1 = pmedio - izquierda + 1; // Tama�o de la primera mitad del arreglo
    int n2 = derecha - pmedio;       // Tama�o de la segunda mitad del arreglo

    // Crear los subarreglos temporales para almacenar las mitades del arreglo original
    vector<int> L(n1), R(n2);

    // Copiar elementos de la mitad izquierda en el subarreglo L
    for (int i = 0; i < n1; i++)
        L[i] = arr[izquierda + i];

    // Copiar elementos de la mitad derecha en el subarreglo R
    for (int i = 0; i < n2; i++)
        R[i] = arr[pmedio + 1 + i];

    // Inicializar �ndices para recorrer los subarreglos y el arreglo original
    int i = 0, j = 0, k = izquierda;

    // Fusionar los dos subarreglos en el arreglo original de forma ordenada
    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) { // Si el elemento en L es menor o igual que el de R
            arr[k++] = L[i++]; // Insertamos el elemento de L en arr y avanzamos en L
        } else {
            arr[k++] = R[j++]; // Insertamos el elemento de R en arr y avanzamos en R
        }
    }

    // Si quedan elementos en L, copiarlos al arreglo original
    while (i < n1)
        arr[k++] = L[i++];

    // Si quedan elementos en R, copiarlos al arreglo original
    while (j < n2)
        arr[k++] = R[j++];
}

// Funci�n principal para recibir el arreglo desde el usuario y ordenarlo
int main() {
    int n;

    // Solicitar al usuario el n�mero de elementos del arreglo
    cout << "Ingrese el numero de elementos del arreglo: ";
    cin >> n;

    // Validaci�n: el n�mero de elementos debe ser positivo
    if (n <= 0) {
        cout << "El n�mero de elementos debe ser positivo." << endl;
        return 1; // Termina la ejecuci�n del programa si el n�mero no es v�lido
    }

    // Crear un vector con el tama�o ingresado por el usuario
    vector<int> arr(n);

    // Solicitar los elementos del arreglo al usuario
    cout << "Ingrese los elementos del arreglo: ";
    for (int i = 0; i < n; i++)
        cin >> arr[i];

    // Llamar a la funci�n mergeSort para ordenar el arreglo
    mergeSort(arr, 0, arr.size() - 1);

    // Mostrar el arreglo ordenado
    cout << "Arreglo ordenado: ";
    for (int num : arr)
        cout << num << " ";
    cout << endl;

    return 0; // Termina el programa correctamente
}
