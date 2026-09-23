000100*** EDIT ALLOWED                                                          
000132*KONSTANTER FÖR BERÄKNING AV EV FLYGBEHOV                                 
000205                                                                          
000342 01  FILLER                             PIC X(16)                         
000442                                        VALUE 'FLYG-TABELL'.              
000505*                                                                         
001000                                                                          
001133 01  DC-FLYGTID.                                                          
001224                                                                          
002742     03 DC-21                           PIC X(4)   VALUE '0302'.          
002942     03 DC-23                           PIC X(4)   VALUE '0302'.          
003042     03 DC-24                           PIC X(4)   VALUE '0302'.          
003142     03 DC-25                           PIC X(4)   VALUE '0302'.          
003242     03 DC-26                           PIC X(4)   VALUE '0302'.          
003342     03 DC-41                           PIC X(4)   VALUE '0506'.          
003442     03 DC-42                           PIC X(4)   VALUE '0610'.          
003542     03 DC-43                           PIC X(4)   VALUE '0610'.          
003643     03 DC-51                           PIC X(4)   VALUE '1510'.          
003742     03 DC-61                           PIC X(4)   VALUE '1015'.          
003844     03 DC-62                           PIC X(4)   VALUE '1007'.          
003845     03 DC-71                           PIC X(4)   VALUE '1515'.          
003846     03 DC-72                           PIC X(4)   VALUE '2515'.          
003847     03 DC-73                           PIC X(4)   VALUE '1515'.          
003848     03 DC-7A                           PIC X(4)   VALUE '1515'.          
003849     03 DC-7B                           PIC X(4)   VALUE '1515'.          
003850     03 DC-7C                           PIC X(4)   VALUE '1515'.          
003851     03 DC-7D                           PIC X(4)   VALUE '1515'.          
003852     03 DC-7E                           PIC X(4)   VALUE '1515'.          
003853     03 DC-7F                           PIC X(4)   VALUE '1515'.          
003854     03 DC-7G                           PIC X(4)   VALUE '1515'.          
003855     03 DC-7H                           PIC X(4)   VALUE '1515'.          
003938                                                                          
004038                                                                          
004138 01  FILLER REDEFINES DC-FLYGTID.                                         
004240     03  DC-FLYGT OCCURS 22.                                              
004338*X                                                                        
004438         05  DC-FLYGT-KVDAGAR-BEHOV     PIC 9(2).                         
004538*Y                                                                        
004638         05  DC-FLYGT-KVDAGAR-ETA       PIC 9(2).                         
005026                                                                          
006124                                                                          
006232* EFTER ÄNDRING AV FLYGTID KOMPILERA OM                                   
006344* W2711000                                                                
006444* W271REFL                                                                
006544* W2222200                                                                
006545* W6115900                                                                
006644*                                                                         
007044* 060331 JN                                                               
