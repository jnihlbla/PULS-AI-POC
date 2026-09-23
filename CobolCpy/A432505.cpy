000100 01  A432505.                                                             
000200*                        *************************************            
000300*                        *** POSTTYP 505                   ***            
000400*                        *** MASKINELLA FAKTURARADER       ***            
000500*                        *** FRÅN KONCERN                  ***            
000600*                        *************************************            
000700     03  PTYP                    PIC X(3).                                
000800*                        ***  POSTTYP 505                                 
000900     03  ARTNR                   PIC X(10).                               
001000*                        ***  ARTIKELNR                                   
001100     03  LEVNR                   PIC X(6).                                
001200*                        ***  LEVNR = 1620 VOLVO KOMP MOTORDIV VSV        
001300*                        ***          1165 VOLVO KOMP TRANSDIV VBV        
001400*                        ***          1555 VOLVO PV OLOFSTRÖM  VOV        
001500*                        ***          1540 VOLVO LV UMEÅ       VUV        
001600     03  ANTAL                   PIC S9(7).                               
001700     03  ANTAL-X   REDEFINES ANTAL                                        
001800                                 PIC X(7).                                
001900*                        ***  ANTAL                                       
002000     03  SORT                    PIC X(2).                                
002100*                        ***  SORT = 00 ELLER 01 = ST                     
002200*                        ***         04 = METER                           
002300*                        ***         05 = KILO                            
002400*                        ***         06 = KVADRATMETER                    
002500*                        ***         07 = FOT                             
002600*                        ***         09 = LITER                           
002700                                                                          
002800     03  DATUM-AVS               PIC X(8).                                
002900*                        ***  AVSÄNDNINGSDATUM SSÅÅMMDD                   
003000     03  PACKNR                  PIC X(6).                                
003100*                        ***  KALLAS ÄVEN FÖLJESEDELNR                    
003200     03  FTAG-GODSMOT            PIC X(2).                                
003300*                        ***  GODSMOTTAGANDE FÖRETAG I GBG                
003400*                        ***  FTAG = 02 VLV                               
003500*                        ***         03 VLV/PARTS                         
003600     03  PRIS-FR                 PIC S9(9)V9(2).                          
003700     03  PRIS-FR-X REDEFINES PRIS-FR                                      
003800                                 PIC X(11).                               
003900*                        ***  FAKTURARADSPRIS                             
004000     03  ENHET-PRIS              PIC X.                                   
004100*                        ***  ENHETSKOD 0 OCH 1 = PER ST                  
004200*                        ***            2 = PER 100                       
004300*                        ***            3 = PER 1000                      
004400*                        ***            8 = PER GROSS                     
004500     03  BEL-FR                  PIC S9(9)V9(2).                          
004600     03  BEL-FR-X REDEFINES BEL-FR                                        
004700                                 PIC X(11).                               
004800*                        ***  FAKTURARADSBELOPP                           
004900*** END OF VILMAII-COPY LENGTH= 67 OLD LENGTH= 67                         
