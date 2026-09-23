000100 01  W51352.                                                              
000200*                                 COPYTEXT TILL SOP PARAMETRAR            
000300*                                                                         
000400     03 BAS                  PIC X(4).                                    
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 SOK-TYP              PIC X(10).                                   
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 KDINVKAT             PIC S9(3)           COMP-3.                  
001100*                                 INVENTERINGSKATEGORI                    
001200     03 DATUM                PIC 9(6).                                    
001300*                                 DATUM ENLIGT KDDATFORM                  
001400     03 IDUSER               PIC X(8).                                    
001500*                                 ANVÄNDARENS SÄKERHETS ID                
001600     03 FILLER               PIC X(43).                                   
001700*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
