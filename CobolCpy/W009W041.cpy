000010 01  W041-W009W041.                                                       
000020*                                 COPYTEXT TILL PROGRAM W0090400          
000030*                                 ANVÄNDS FÖR ATT SÖKA OM ETT ORD         
000040*                                 FINNS I EN TEXTMASSA.  CALL             
000050*                                 W009LTXT USING W041-W009W041            
000060     03 W041-BESTEXT         PIC X(330).                                  
000070*                                 TEXT DÄR SÖKNING SKER                   
000080     03 W041-DIFAELT         PIC S9(4)           COMP.                    
000090*                                 FÄLTLÄNGD                               
000100     03 W041-BESORD          PIC X(50).                                   
000110*                                 ORD SOM SKALL SÖKAS                     
000120     03 W041-KDSVAR          PIC X.                                       
000130      88 W041-OK             VALUE ' '.                                   
000140      88 W041-SAKNAS         VALUE 'S'.                                   
000150      88 W041-FEL            VALUE 'F'.                                   
000160*                                                       KDSVAR-88         
000170*                                 SVARSKOD FRÅN SUBPROGRAM                
      *** END COPY W009W041    LENGTH=383                                       
