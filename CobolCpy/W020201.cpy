000010 01  W020201.                                                             
000020*                                 POST FRÅN WDG6.                         
000030*                                 POSTTYP 201 = DEL-/ANNULLERAD           
000040*                                               RESTORDER.                
000050     03 IDPTYP               PIC X(3).                                    
000060*                                 POSTTYP                                 
000070     03 IDDISTR              PIC S9(5)           COMP-3.                  
000080*                                 DISTRIKTNUMMER                          
000090     03 IDORDER              PIC S9(7)           COMP-3.                  
000100*                                 VOLVO PARTS ORDERNUMMER                 
000110     03 IDARTNR              PIC S9(9)           COMP-3.                  
000120*                                 ARTIKELNUMMER                           
000130     03 KVLEVART             PIC S9(7)           COMP-3.                  
000140*                                 LEVERERAT ANTAL STYCK                   
      *** END COPY W020201     LENGTH=19                                        
