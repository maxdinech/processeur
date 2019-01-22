Instructions :

	Increment
		Contrôle : 1100
		Mot : rs 6 rt 6
		Fonctionnement : rt = rs + 1
	
	Load_word
		Contrôle : 0011
		Mot : rs 6 val 6
		Fonctionnement : rs est affecté avec la valeur de val

	Modulo 4 (100/400)
		Contrôle : 1110 (1000 1001)
		Mot : rs 6 rt 6
		Fonctionnement : rt = rs % 4/100/400

	Comm_in
		Contrôle : 0101
		Mot : rs 6 rt 6
		Fonctionnement : gérer une entrée et faire des inscriptions dans des registres spéciaux qui serviront à stocker les paramètres courants

	Comm_out
		Contrôle : 0110
		Mot : rs 6 rt 6
		Fonctionnement : transmettre des choses vers l’extérieur (affichage)

	Display
		Contrôle : 0111
		Mot : rs 6 rt 6
		Fonctionnement : mettre à jour un affichage spécifique (à voir avec comm_out)

	Test_equal
		Contrôle : 0001
		Mot : rs 6 rt 6
		Fonctionnement : si rs et rt sont égaux, on saute une instruction dans le PC, sinon on continue l’exécution normalement et on passe à la prochaine instruction.

	Jump
		Contrôle : 0000
		Mot : rs 12
		Fonctionnement : On saute à l’instruction concernée (4096 lignes possibles)	
