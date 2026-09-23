000100 01  REQU-W60182I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W6018200              
000300*                                                                         
000400     03 REQU-IDARTNR-KEY     PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 REQU-IDDC-KEY        PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 REQU-IDARTNR-KOPI    PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 REQU-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 REQU-EMB-Q.                                                       
001300*                                 RADER SOM VISAR EMBALLAGE ARTIK         
001400*                                 LAR                                     
001500        05 REQU-IDARTNR-EMBQ0                                             
001600                             PIC X(9).                                    
001700*                                 EMBALLAGEARTIKELNR FÖR Q0               
001800        05 REQU-KVQPACK-EMBQ0                                             
001900                             PIC X(5).                                    
002000*                                 ANTAL I Q0 FÖRPACKNING                  
002100        05 REQU-KDEMBKOD-EMBQ0                                            
002200                             PIC X(3).                                    
002300*                                 EMBALLAGEKOD 0                          
002400        05 REQU-IDARTNR-EMBQ1                                             
002500                             PIC X(9).                                    
002600*                                 EMBALLAGEARTIKELNR FÖR Q1               
002700        05 REQU-KVQPACK-EMBQ1                                             
002800                             PIC X(5).                                    
002900*                                 ANTAL I Q1 FÖRPACKNING                  
003000        05 REQU-KDEMBKOD-EMBQ1                                            
003100                             PIC X(3).                                    
003200*                                 EMBALLAGEKOD 1                          
003300        05 REQU-IDARTNR-EMBQ2                                             
003400                             PIC X(9).                                    
003500*                                 EMBALLAGEARTIKELNR FÖR Q2               
003600        05 REQU-KVQPACK-EMBQ2                                             
003700                             PIC X(5).                                    
003800*                                 ANTAL I Q2 FÖRPACKNING                  
003900        05 REQU-KDEMBKOD-EMBQ2                                            
004000                             PIC X(3).                                    
004100*                                 EMBALLAGEKOD 2                          
004200        05 REQU-KVQPACK-EMBQ3                                             
004300                             PIC X(5).                                    
004400*                                 ANTAL I Q3 FÖRPACKNING                  
004500        05 REQU-KVQPACK-EMBQ4                                             
004600                             PIC X(5).                                    
004700*                                 ANTAL I Q4 FÖRPACKNING                  
004800     03 REQU-TIUPPDAT-IN     PIC 9(6).                                    
004900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
005000     03 REQU-IDUSER-IN       PIC X(8).                                    
005100*                                 ANVÄNDARENS SÄKERHETS ID                
005200     03 REQU-EMB-X           OCCURS 10 TIMES.                             
005300*                                 RADER SOM VISAR EXTRA EMBALLAGE         
005400*                                  ARTIKLAR                               
005500        05 REQU-IDARTNR-EMBX PIC X(9).                                    
005600*                                 EMBALLAGE-ARTIKELNUMMER                 
005700        05 REQU-KVQPACK-EMBX PIC X(5).                                    
005800*                                 ANTAL I FÖRPACKNING                     
005900*                                 GÄLLER FÖR EXTRAEMBALLAGEN              
006000*** END OF VILMAII-COPY LENGTH= 237 BYTES                                 
