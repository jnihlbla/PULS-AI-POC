000100 01  W222RP5T.                                                            
000200*                                 UPPDATERING CENTRAL PBJUSTERING         
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC 9(8).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 KDCLAGER             PIC 9.                                       
000900*                                 CENTRALLAGERKOD                         
001000     03 TIPBJUST             PIC 9(4).                                    
001100*                                 DATUM F÷R PB-JUSTERING (≈≈VV)           
001200     03 REPBJUST             PIC 9V9(2).                                  
001300*                                 JUSTERINGSFAKTOR-PB                     
001400     03 FLABORT-JUST         PIC X.                                       
001500*                                 BORTTAG AV BEFINTLIG                    
001600*                                 PB-JUSTERING?                           
001700*** END COPY W222RP5TC0  LENGTH=20                                        
