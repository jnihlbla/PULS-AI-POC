000100 01  W44035.                                                              
000200*                                 FIL FRÅN LAGERBAND MED KVROS >          
000300*                                 0                                       
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 KVROS-DAG            PIC S9(7)           COMP-3.                  
000800*                                 RESTORDERSALDO, KLASS 1                 
000900*                                 BACK ORDER BALANCE, CLASS 1             
001000     03 KVROS-BULK           PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERSALDO, KLASS 2-4               
001200*                                 BACK ORDER BALANCE, CLASS 2-4           
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
