000100 01  MOD-W6O31901.                                                        
000200*                                 COPYTEXT FÖR MOD W6031901               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDARTNR-IN       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 MOD-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 MOD-KDLOC-IN         PIC X.                                       
001700*                                 TYP AV LAGERPLATS                       
001800*                                 TYPE OF LOCATION                        
001900     03 MOD-ARTIKEL-UT.                                                   
002000        05 MOD-IDARTNR-UT    PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300        05 MOD-STRECK        PIC X.                                       
002400        05 MOD-REKSIFFR      PIC 9.                                       
002500*                                 KONTROLLSIFFRA                          
002600*                                 PART NO CHECK DIGIT                     
002700     03 MOD-IDDC-UT          PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900*                                 WAREHOUSE IDENTIFIER                    
003000     03 MOD-KDLOC-UT         PIC X.                                       
003100*                                 TYP AV LAGERPLATS                       
003200*                                 TYPE OF LOCATION                        
003300     03 MOD-BEART            PIC X(25).                                   
003400*                                 ARTIKELBENÄMNING                        
003500*                                 PART DESCRIPTION                        
003600     03 MOD-RAD              OCCURS 13 TIMES.                             
003700        05 MOD-TISTADAT      PIC 9(6).                                    
003800*                                 GENERELLT STARTDATUM                    
003900*                                 GENERAL START DATE                      
004000        05 MOD-ADLAGOMR      PIC Z9.                                      
004100*                                 LAGEROMRÅDE                             
004200*                                 AREA                                    
004300        05 MOD-ADGANG        PIC Z9.                                      
004400*                                 GÅNG                                    
004500*                                 AISLE                                   
004600        05 MOD-ADPLATS       PIC Z(4)9.                                   
004700*                                 LAGERPLATSNUMMER                        
004800*                                 LOCATION                                
004900        05 MOD-TISTODAT      PIC 9(6).                                    
005000*                                 GENERELLT STOPPDATUM                    
005100*                                 GENERAL STOP DATE YYMMDD                
005200        05 MOD-KDLOC         PIC X.                                       
005300*                                 TYP AV LAGERPLATS                       
005400*                                 TYPE OF LOCATION                        
005500        05 MOD-IDUSER        PIC X(8).                                    
005600*                                 ANVÄNDARENS SÄKERHETS ID                
005700*                                 USER SECURITY-IDENTITY                  
005800        05 MOD-IDUSER-STO    PIC X(8).                                    
005900*                                 ANVÄNDARENS SÄKERHETS ID                
006000*                                 USER SECURITY-IDENTITY                  
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*                                 INFORMATION MESSAGE                     
006400*** END OF VILMAII-COPY LENGTH= 644 BYTES                                 
