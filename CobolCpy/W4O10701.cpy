000100 01  W4O10701.                                                            
000200*                                 COPYTEXT FÖR MOD W4O10701               
000300*                                                                         
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MESSAGE-RAD1         PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 IDKAT-SKIP           PIC 9(3).                                    
000900*                                 ANTAL IDKAT SOM LÄSES FÖRBI             
001000     03 IDARTNR-IN           PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 IDARTNR-UT           PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 STRECK               PIC X.                                       
001500     03 REKSIFFR             PIC 9.                                       
001600*                                 KONTROLLSIFFRA                          
001700     03 IDDC-IN              PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 IDDC-UT              PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 AREA.                                                             
002200*                                 AREA SOM NOLLSTÄLLS MELLAN              
002300*                                 VARVEN                                  
002400        05 BEART-SVE-ENG     PIC X(25).                                   
002500*                                 ARTIKELBENÄMNING                        
002600        05 BELEV             PIC X(30).                                   
002700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002800        05 IDARTNR-EMBQ0     PIC Z(8)9.                                   
002900*                                 EMBALLAGEARTIKELNR FÖR Q0               
003000        05 KVQPACK-0         PIC Z(4)9.                                   
003100*                                 ANTAL I Q0 FÖRPACKNING                  
003200        05 IDARTNR-EMBQ1     PIC Z(8)9.                                   
003300*                                 EMBALLAGEARTIKELNR FÖR Q1               
003400        05 KVQPACK-1         PIC Z(4)9.                                   
003500*                                 ANTAL I Q1 FÖRPACKNING                  
003600        05 IDARTNR-EMBQ2     PIC Z(8)9.                                   
003700*                                 EMBALLAGEARTIKELNR FÖR Q2               
003800        05 KVQPACK-2         PIC Z(4)9.                                   
003900*                                 ANTAL I Q2 FÖRPACKNING                  
004000        05 IDARTNR-EMBQ3     PIC Z(8)9.                                   
004100*                                 EMBALLAGEARTIKELNR FÖR Q3               
004200        05 KVQPACK-3         PIC Z(4)9.                                   
004300*                                 ANTAL I Q3 FÖRPACKNING                  
004400        05 IDARTNR-EMBQ4     PIC Z(8)9.                                   
004500*                                 EMBALLAGEARTIKELNR FÖR Q4               
004600        05 KVQPACK-4         PIC Z(4)9.                                   
004700*                                 ANTAL I Q4 FÖRPACKNING                  
004800        05 BEFT              PIC Z(2)9.                                   
004900*                                 FÖRPACKNINGSTYP                         
005000        05 IDKAT             OCCURS 16 TIMES                              
005100                             PIC X(6).                                    
005200*                                 KATALOGTILLHÖRIGHET   IDKAT-002         
005300        05 KDPRODSL          PIC Z(2)9.                                   
005400*                                 PRODUKTSLAG                             
005500        05 CDC-VKART         PIC Z(6)9.9(2).                              
005600*                                 ARTIKELVIKT (G/OZ)                      
005700        05 DC-VKART          PIC Z(6)9.9(2).                              
005800*                                 ARTIKELVIKT (G/OZ)                      
005900        05 BESORT-VKART      PIC X(6).                                    
006000*                                 BENÄMNING PÅ SORT/ENHET                 
006100        05 CDC-VLARTNTO      PIC Z(7)9.9.                                 
006200*                                 ARTIKELVOLYM NETTO (CM3)                
006300        05 DC-VLARTNTO       PIC Z(7)9.9.                                 
006400*                                 ARTIKELVOLYM NETTO (CM3)                
006500        05 BESORT-VLARTNTO   PIC X(6).                                    
006600*                                 BENÄMNING PÅ SORT/ENHET                 
006700        05 CDC-KDARTURS      PIC X(2).                                    
006800*                                 ARTIKELURSPRUNGSKOD                     
006900        05 DC-KDARTURS       PIC X(2).                                    
007000*                                 ARTIKELURSPRUNGSKOD                     
007100        05 IDAO              OCCURS 5 TIMES                               
007200                             PIC X(11).                                   
007300*                                 ÄNDRINGSORDERNUMMER                     
007400        05 MESSAGE-RAD23     PIC X(79).                                   
007500*                                 MEDDELANDEFÄLT PÅ RAD 23                
007600*** END OF VILMAII-COPY LENGTH= 488 BYTES                                 
