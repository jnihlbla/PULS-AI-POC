000100 01  W020204.                                                             
000200*                                 POST FR≈N WDG6.                         
000300*                                 POSTTYP 204 = ORDER SOM BIPAC-          
000400*                                               KATS HELT ELLER           
000500*                                               DELVIS.                   
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDORDER              PIC S9(7)           COMP-3.                  
001100*                                 VOLVO PARTS ORDERNUMMER                 
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 TIRODAT              PIC S9(7)           COMP-3.                  
001500*                                 RESTORDERDATUM         (≈≈MMDD)         
001600     03 KVLEVART             PIC S9(7)           COMP-3.                  
001700*                                 LEVERERAT ANTAL ARTIKLAR                
001800*** END COPY W020204     LENGTH=23                                        
