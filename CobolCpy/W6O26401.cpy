000100 01  W6O26401.                                                            
000200*                                                                         
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE-RAD1         PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 STRECK               PIC X.                                       
001200     03 REKSIFFR             PIC 9.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 GAMMAL-AREA.                                                      
001500        05 IDARTNR-EMBQ0-OLD-ATTR                                         
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 IDARTNR-EMBQ0-OLD PIC Z(7)9.                                   
001900*                                 EMBALLAGEARTIKELNR FÖR Q0               
002000        05 KDEMBKOD-0-OLD-ATTR                                            
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 KDEMBKOD-0-OLD    PIC Z(2)9.                                   
002400*                                 EMBALLAGEKOD 0                          
002500        05 IDARTNR-EMBQ1-OLD-ATTR                                         
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 IDARTNR-EMBQ1-OLD PIC Z(7)9.                                   
002900*                                 EMBALLAGEARTIKELNR FÖR Q1               
003000        05 KDEMBKOD-1-OLD-ATTR                                            
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 KDEMBKOD-1-OLD    PIC Z(2)9.                                   
003400*                                 EMBALLAGEKOD 1                          
003500        05 IDARTNR-EMBQ2-OLD-ATTR                                         
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 IDARTNR-EMBQ2-OLD PIC Z(7)9.                                   
003900*                                 EMBALLAGEARTIKELNR FÖR Q2               
004000        05 KDEMBKOD-2-OLD-ATTR                                            
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 KDEMBKOD-2-OLD    PIC Z(2)9.                                   
004400*                                 EMBALLAGEKOD 2                          
004500        05 BEFT-OLD-ATTR     PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 BEFT-OLD          PIC Z9.                                      
004800*                                 FÖRPACKNINGSTYP                         
004900        05 KVQPACK-0-OLD-ATTR                                             
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 KVQPACK-0-OLD     PIC Z(4)9.                                   
005300*                                 ANTAL I Q0 FÖRPACKNING                  
005400        05 KDFORP-OLD-ATTR   PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 KDFORP-OLD        PIC Z(3)9.                                   
005700*                                 FÖRPACKNINGSKOD                         
005800        05 KVQPACK-2-OLD-ATTR                                             
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 KVQPACK-2-OLD     PIC Z(4)9.                                   
006200*                                 ANTAL I Q2 FÖRPACKNING                  
006300     03 NY-AREA              OCCURS 10 TIMES.                             
006400        05 NEW-ATTR          PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 NEW               PIC X(2).                                    
006700*                                 MFS BEHANDLING AV INPUTFÄLT             
006800     03 MESSAGE-RAD23        PIC X(79).                                   
006900*                                 MEDDELANDEFÄLT PÅ RAD 23                
