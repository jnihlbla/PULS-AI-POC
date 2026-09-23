000100 01  MOD-W6O18201.                                                        
000200*                                 MOD FÖR PROGRAM W60182                  
000300*                                 PROGRAMMET VISAR OCH UPPDATERAR         
000400*                                 FP-EMBALLAGE                            
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-KEY-IN      PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDDC-KEY-UT      PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDARTNR-KOPI-ATTR                                             
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-IDARTNR-KOPI     PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-BEFT-UT          PIC X(2).                                    
002300*                                 FÖRPACKNINGSTYP                         
002400     03 MOD-KDFORP-UT.                                                    
002500*                                 FÖRPACKNINGSKOD                         
002600        05 MOD-KDFORPPL      PIC 9.                                       
002700*                                 FÖRPACKNINGSPLATS                       
002800        05 MOD-KDFORPGP      PIC 9(2).                                    
002900*                                 FÖRPACKNINGSGRUPP                       
003000        05 MOD-KDFORPUF      PIC 9.                                       
003100*                                 UPPRÄKNINGSFAKTOR                       
003200     03 MOD-FLFPINST-UPP     PIC X.                                       
003300*                                 ALLMÄN FLAGGA                           
003400     03 MOD-TIUPPDAT-UT      PIC X(6).                                    
003500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003600     03 MOD-IDUSER-UT        PIC X(8).                                    
003700*                                 ANVÄNDARENS SÄKERHETS ID                
003800     03 MOD-EMB-Q.                                                        
003900*                                 RADER SOM VISAR EMBALLAGE ARTIK         
004000*                                 LAR                                     
004100        05 MOD-IDARTNR-EMBQ0-UT                                           
004200                             PIC X(9).                                    
004300*                                 EMBALLAGEARTIKELNR FÖR Q0               
004400        05 MOD-IDARTNR-EMBQ0-IN-ATTR                                      
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-IDARTNR-EMBQ0-IN                                           
004800                             PIC X(9).                                    
004900*                                 EMBALLAGEARTIKELNR FÖR Q0               
005000        05 MOD-KVQPACK-EMBQ0-UT                                           
005100                             PIC X(5).                                    
005200*                                 ANTAL I Q0 FÖRPACKNING                  
005300        05 MOD-KVQPACK-EMBQ0-IN-ATTR                                      
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-KVQPACK-EMBQ0-IN                                           
005700                             PIC X(5).                                    
005800*                                 ANTAL I Q0 FÖRPACKNING                  
005900        05 MOD-KDEMBKOD-EMBQ0-UT                                          
006000                             PIC X(3).                                    
006100*                                 EMBALLAGEKOD 0                          
006200        05 MOD-KDEMBKOD-EMBQ0-IN-ATTR                                     
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-KDEMBKOD-EMBQ0-IN                                          
006600                             PIC X(3).                                    
006700*                                 EMBALLAGEKOD 0                          
006800        05 MOD-IDARTNR-EMBQ1-UT                                           
006900                             PIC X(9).                                    
007000*                                 EMBALLAGEARTIKELNR FÖR Q1               
007100        05 MOD-IDARTNR-EMBQ1-IN-ATTR                                      
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-IDARTNR-EMBQ1-IN                                           
007500                             PIC X(9).                                    
007600*                                 EMBALLAGEARTIKELNR FÖR Q1               
007700        05 MOD-KVQPACK-EMBQ1-UT                                           
007800                             PIC X(5).                                    
007900*                                 ANTAL I Q1 FÖRPACKNING                  
008000        05 MOD-KVQPACK-EMBQ1-IN-ATTR                                      
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-KVQPACK-EMBQ1-IN                                           
008400                             PIC X(5).                                    
008500*                                 ANTAL I Q1 FÖRPACKNING                  
008600        05 MOD-KDEMBKOD-EMBQ1-UT                                          
008700                             PIC X(3).                                    
008800*                                 EMBALLAGEKOD 1                          
008900        05 MOD-KDEMBKOD-EMBQ1-IN-ATTR                                     
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-KDEMBKOD-EMBQ1-IN                                          
009300                             PIC X(3).                                    
009400*                                 EMBALLAGEKOD 1                          
009500        05 MOD-IDARTNR-EMBQ2-UT                                           
009600                             PIC X(9).                                    
009700*                                 EMBALLAGEARTIKELNR FÖR Q2               
009800        05 MOD-IDARTNR-EMBQ2-IN-ATTR                                      
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-IDARTNR-EMBQ2-IN                                           
010200                             PIC X(9).                                    
010300*                                 EMBALLAGEARTIKELNR FÖR Q2               
010400        05 MOD-KVQPACK-EMBQ2-UT                                           
010500                             PIC X(5).                                    
010600*                                 ANTAL I Q2 FÖRPACKNING                  
010700        05 MOD-KVQPACK-EMBQ2-IN-ATTR                                      
010800                             PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000        05 MOD-KVQPACK-EMBQ2-IN                                           
011100                             PIC X(5).                                    
011200*                                 ANTAL I Q2 FÖRPACKNING                  
011300        05 MOD-KDEMBKOD-EMBQ2-UT                                          
011400                             PIC X(3).                                    
011500*                                 EMBALLAGEKOD 2                          
011600        05 MOD-KDEMBKOD-EMBQ2-IN-ATTR                                     
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900        05 MOD-KDEMBKOD-EMBQ2-IN                                          
012000                             PIC X(3).                                    
012100*                                 EMBALLAGEKOD 2                          
012200        05 MOD-KVQPACK-EMBQ3-UT                                           
012300                             PIC X(5).                                    
012400*                                 ANTAL I Q3 FÖRPACKNING                  
012500        05 MOD-KVQPACK-EMBQ3-IN-ATTR                                      
012600                             PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800        05 MOD-KVQPACK-EMBQ3-IN                                           
012900                             PIC X(5).                                    
013000*                                 ANTAL I Q3 FÖRPACKNING                  
013100        05 MOD-KVQPACK-EMBQ4-UT                                           
013200                             PIC X(5).                                    
013300*                                 ANTAL I Q4 FÖRPACKNING                  
013400        05 MOD-KVQPACK-EMBQ4-IN-ATTR                                      
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700        05 MOD-KVQPACK-EMBQ4-IN                                           
013800                             PIC X(5).                                    
013900*                                 ANTAL I Q4 FÖRPACKNING                  
014000     03 MOD-EMB-X            OCCURS 10 TIMES.                             
014100*                                 RADER SOM VISAR EXTRA EMBALLAGE         
014200*                                  ARTIKLAR                               
014300        05 MOD-IDARTNR-EMBX-UT                                            
014400                             PIC X(9).                                    
014500*                                 EMBALLAGE-ARTIKELNUMMER                 
014600        05 MOD-IDARTNR-EMBX-IN-ATTR                                       
014700                             PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900        05 MOD-IDARTNR-EMBX-IN                                            
015000                             PIC X(9).                                    
015100*                                 EMBALLAGE-ARTIKELNUMMER                 
015200        05 MOD-KVQPACK-EMBX-UT                                            
015300                             PIC X(5).                                    
015400*                                 ANTAL I FÖRPACKNING                     
015500*                                 GÄLLER FÖR EXTRAEMBALLAGEN              
015600        05 MOD-KVQPACK-EMBX-IN-ATTR                                       
015700                             PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900        05 MOD-KVQPACK-EMBX-IN                                            
016000                             PIC X(5).                                    
016100*                                 ANTAL I FÖRPACKNING                     
016200*                                 GÄLLER FÖR EXTRAEMBALLAGEN              
016300     03 MOD-TEMFSINF         PIC X(55).                                   
016400*                                 INFORMATIONSMEDDELANDE                  
016500*** END OF VILMAII-COPY LENGTH= 617 BYTES                                 
