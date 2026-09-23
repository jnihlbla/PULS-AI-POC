000100 01  RESP-W60182O1.                                                       
000200*                                 MOD FÖR PROGRAM W60182                  
000300*                                 PROGRAMMET VISAR OCH UPPDATERAR         
000400*                                 FP-EMBALLAGE                            
000500     03 RESP-IDDC-KEY        PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 RESP-IDARTNR-KOPI-ATTR                                            
000800                             PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 RESP-IDARTNR-KOPI    PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 RESP-BEFT            PIC X(2).                                    
001300*                                 FÖRPACKNINGSTYP                         
001400     03 RESP-KDFORP.                                                      
001500*                                 FÖRPACKNINGSKOD                         
001600        05 RESP-KDFORPPL     PIC 9.                                       
001700*                                 FÖRPACKNINGSPLATS                       
001800        05 RESP-KDFORPGP     PIC 9(2).                                    
001900*                                 FÖRPACKNINGSGRUPP                       
002000        05 RESP-KDFORPUF     PIC 9.                                       
002100*                                 UPPRÄKNINGSFAKTOR                       
002200     03 RESP-FLFPINST-UPP    PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400     03 RESP-TIUPPDAT        PIC X(6).                                    
002500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002600     03 RESP-IDUSER-ATTR     PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 RESP-IDUSER          PIC X(8).                                    
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000     03 RESP-EMB-Q.                                                       
003100*                                 RADER SOM VISAR EMBALLAGE ARTIK         
003200*                                 LAR                                     
003300        05 RESP-IDARTNR-EMBQ0-UT                                          
003400                             PIC X(9).                                    
003500*                                 EMBALLAGEARTIKELNR FÖR Q0               
003600        05 RESP-IDARTNR-EMBQ0-ATTR                                        
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 RESP-IDARTNR-EMBQ0                                             
004000                             PIC X(9).                                    
004100*                                 EMBALLAGEARTIKELNR FÖR Q0               
004200        05 RESP-KVQPACK-EMBQ0-UT                                          
004300                             PIC X(5).                                    
004400*                                 ANTAL I Q0 FÖRPACKNING                  
004500        05 RESP-KVQPACK-EMBQ0-ATTR                                        
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 RESP-KVQPACK-EMBQ0                                             
004900                             PIC X(5).                                    
005000*                                 ANTAL I Q0 FÖRPACKNING                  
005100        05 RESP-KDEMBKOD-EMBQ0-UT                                         
005200                             PIC X(3).                                    
005300*                                 EMBALLAGEKOD 0                          
005400        05 RESP-KDEMBKOD-EMBQ0-ATTR                                       
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 RESP-KDEMBKOD-EMBQ0                                            
005800                             PIC X(3).                                    
005900*                                 EMBALLAGEKOD 0                          
006000        05 RESP-IDARTNR-EMBQ1-UT                                          
006100                             PIC X(9).                                    
006200*                                 EMBALLAGEARTIKELNR FÖR Q1               
006300        05 RESP-IDARTNR-EMBQ1-ATTR                                        
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 RESP-IDARTNR-EMBQ1                                             
006700                             PIC X(9).                                    
006800*                                 EMBALLAGEARTIKELNR FÖR Q1               
006900        05 RESP-KVQPACK-EMBQ1-UT                                          
007000                             PIC X(5).                                    
007100*                                 ANTAL I Q1 FÖRPACKNING                  
007200        05 RESP-KVQPACK-EMBQ1-ATTR                                        
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 RESP-KVQPACK-EMBQ1                                             
007600                             PIC X(5).                                    
007700*                                 ANTAL I Q1 FÖRPACKNING                  
007800        05 RESP-KDEMBKOD-EMBQ1-UT                                         
007900                             PIC X(3).                                    
008000*                                 EMBALLAGEKOD 1                          
008100        05 RESP-KDEMBKOD-EMBQ1-ATTR                                       
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 RESP-KDEMBKOD-EMBQ1                                            
008500                             PIC X(3).                                    
008600*                                 EMBALLAGEKOD 1                          
008700        05 RESP-IDARTNR-EMBQ2-UT                                          
008800                             PIC X(9).                                    
008900*                                 EMBALLAGEARTIKELNR FÖR Q2               
009000        05 RESP-IDARTNR-EMBQ2-ATTR                                        
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 RESP-IDARTNR-EMBQ2                                             
009400                             PIC X(9).                                    
009500*                                 EMBALLAGEARTIKELNR FÖR Q2               
009600        05 RESP-KVQPACK-EMBQ2-UT                                          
009700                             PIC X(5).                                    
009800*                                 ANTAL I Q2 FÖRPACKNING                  
009900        05 RESP-KVQPACK-EMBQ2-ATTR                                        
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 RESP-KVQPACK-EMBQ2                                             
010300                             PIC X(5).                                    
010400*                                 ANTAL I Q2 FÖRPACKNING                  
010500        05 RESP-KDEMBKOD-EMBQ2-UT                                         
010600                             PIC X(3).                                    
010700*                                 EMBALLAGEKOD 2                          
010800        05 RESP-KDEMBKOD-EMBQ2-ATTR                                       
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 RESP-KDEMBKOD-EMBQ2                                            
011200                             PIC X(3).                                    
011300*                                 EMBALLAGEKOD 2                          
011400        05 RESP-KVQPACK-EMBQ3-UT                                          
011500                             PIC X(5).                                    
011600*                                 ANTAL I Q3 FÖRPACKNING                  
011700        05 RESP-KVQPACK-EMBQ3-ATTR                                        
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 RESP-KVQPACK-EMBQ3                                             
012100                             PIC X(5).                                    
012200*                                 ANTAL I Q3 FÖRPACKNING                  
012300        05 RESP-KVQPACK-EMBQ4-UT                                          
012400                             PIC X(5).                                    
012500*                                 ANTAL I Q4 FÖRPACKNING                  
012600        05 RESP-KVQPACK-EMBQ4-ATTR                                        
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900        05 RESP-KVQPACK-EMBQ4                                             
013000                             PIC X(5).                                    
013100*                                 ANTAL I Q4 FÖRPACKNING                  
013200     03 RESP-EMB-X           OCCURS 10 TIMES.                             
013300*                                 RADER SOM VISAR EXTRA EMBALLAGE         
013400*                                  ARTIKLAR                               
013500        05 RESP-IDARTNR-EMBX-UT                                           
013600                             PIC X(9).                                    
013700*                                 EMBALLAGE-ARTIKELNUMMER                 
013800        05 RESP-IDARTNR-EMBX-ATTR                                         
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100        05 RESP-IDARTNR-EMBX PIC X(9).                                    
014200*                                 EMBALLAGE-ARTIKELNUMMER                 
014300        05 RESP-KVQPACK-EMBX-UT                                           
014400                             PIC X(5).                                    
014500*                                 ANTAL I FÖRPACKNING                     
014600*                                 GÄLLER FÖR EXTRAEMBALLAGEN              
014700        05 RESP-KVQPACK-EMBX-ATTR                                         
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000        05 RESP-KVQPACK-EMBX PIC X(5).                                    
015100*                                 ANTAL I FÖRPACKNING                     
015200*                                 GÄLLER FÖR EXTRAEMBALLAGEN              
015300     03 RESP-KDARTURS        PIC X(2).                                    
015400*                                 ARTIKELURSPRUNGSKOD                     
015500*** END OF VILMAII-COPY LENGTH= 502 BYTES                                 
