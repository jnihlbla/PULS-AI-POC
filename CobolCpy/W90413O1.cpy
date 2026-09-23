000100 01  MOD-W90413O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9041300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FILLER           PIC X(9).                                    
000800     03 MOD-STRNR-UT         PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-FILLER           PIC X(3).                                    
001100     03 MOD-FILLER           PIC X(3).                                    
001200     03 MOD-FILLER           PIC X(4).                                    
001300     03 MOD-IDRADNR-UT       PIC X(4).                                    
001400*                                 RADNUMMER                               
001500     03 MOD-IDRADNR-B        PIC X(4).                                    
001600*                                 RADNUMMER                               
001700     03 MOD-FILLER           PIC X(2).                                    
001800     03 MOD-FILLER           PIC X(4).                                    
001900     03 MOD-FILLER           PIC X(2).                                    
002000     03 MOD-FILLER           PIC X(4).                                    
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-FILLER           PIC X(5).                                    
002400     03 MOD-FILLER           PIC X(30).                                   
002500     03 MOD-IDARTNR-IN-ATTR  PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-FILLER           PIC X(9).                                    
002800     03 MOD-FILLER           PIC X(2).                                    
002900     03 MOD-FILLER           PIC X(5).                                    
003000     03 MOD-FILLER           PIC X(2).                                    
003100     03 MOD-FILLER           PIC X(30).                                   
003200     03 MOD-FILLER           PIC X(25).                                   
003300     03 MOD-FILLER           PIC 9.                                       
003400     03 MOD-FILLER           PIC X(2).                                    
003500     03 MOD-FILLER           PIC X(25).                                   
003600     03 MOD-FILLER           PIC X(2).                                    
003700     03 MOD-FILLER           PIC 9.                                       
003800     03 MOD-FILLER           PIC X(5).                                    
003900     03 MOD-FILLER           PIC X(30).                                   
004000     03 MOD-FILLER           PIC 9(6).                                    
004100     03 MOD-REANTPSA-UT      PIC Z9.9(3).                                 
004200*                                 ANTAL PER SATS                          
004300     03 MOD-FILLER           PIC X.                                       
004400     03 MOD-FILLER           PIC X(2).                                    
004500     03 MOD-REANTPSA-IN-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-FILLER           PIC 9(6).                                    
004800     03 MOD-FILLER           PIC X(2).                                    
004900     03 MOD-FILLER           PIC X.                                       
005000     03 MOD-FILLER           PIC X(2).                                    
005100     03 MOD-FILLER           PIC X(2).                                    
005200     03 MOD-IDAO-IN-ATTR     PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-IDAO-IN          PIC X(10).                                   
005500*                                 ÄNDRINGSORDERNUMMER                     
005600     03 MOD-TIAAVV-IN-ATTR   PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-FILLER           PIC 9(4).                                    
005900     03 MOD-TESTRNOT-GRUPP   OCCURS 2 TIMES.                              
006000*                                                                         
006100        05 MOD-TESTRNOT-IN-ATTR                                           
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-TESTRNOT-IN   PIC X(70).                                   
006500*                                 STRUKTURNOTERING                        
006600     03 MOD-KLAR-IN-ATTR     PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-KLAR-IN          PIC X.                                       
006900*                                 ALLMÄN SVARSFLAGGA                      
007000     03 MOD-BORT-IN-ATTR     PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-BORT-IN          PIC X.                                       
007300*                                 ALLMÄN SVARSFLAGGA                      
007400     03 MOD-TEMFSINF         PIC X(55).                                   
007500*                                 INFORMATIONSMEDDELANDE                  
007600*** END OF VILMAII-COPY LENGTH= 530 BYTES                                 
