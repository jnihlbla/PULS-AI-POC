000100 01  W222RP5.                                                             
000200*                                 UPPDATERING CENTRAL PBJUSTERING         
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 KDCLAGER             PIC S9              COMP-3.                  
000900*                                 CENTRALLAGERKOD                         
001000     03 TIPBJUST             PIC S9(5)           COMP-3.                  
001100*                                 DATUM F÷R PB-JUSTERING (≈≈VV)           
001200     03 REPBJUST             PIC S9V9(2)         COMP-3.                  
001300*                                 JUSTERINGSFAKTOR-PB                     
001400     03 FLABORT-JUST         PIC X.                                       
001500*                                 BORTTAG AV BEFINTLIG                    
001600*                                 PB-JUSTERING?                           
001700*** END COPY W222RP5CC0  LENGTH=15                                        
