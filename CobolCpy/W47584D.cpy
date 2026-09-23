000010*** EDIT ALLOWED                                                          
000100 01  W47584D.                                                             
000200*                                 NORTH AMERICAN BROKERS                  
000210*                                 INFORMATION: DETAIL                     
000300*                                                                         
000400     03 IDPTYP               PIC X(1).                                    
000500*                                 RECORD IDENTIFIER 'D'                   
000510     03 IDFAKT               PIC X(10).                                   
000520*                                 INVOICE NUMBER                          
000530     03 IDDISTR              PIC 9(4).                                    
000540*                                 DISTRICT NUMBER                         
000550     03 IDKUNDNR             PIC 9(6).                                    
000560*                                 CUSTOMER NUMBER                         
000570     03 IDPRODNR             PIC 9(7).                                    
000580*                                 PRODUCTION NUMBER                       
000600     03 IDARTNR              PIC X(20).                                   
000700*                                 PART NUMBER                             
000800     03 KVLEVART             PIC 9(5).                                    
000900*                                 QUANTITY DELIVERED                      
001000     03 PRARTNTO             PIC 9(7)V9(2).                               
001100*                                 NET PRICE SEK                           
001200     03 KDSORT               PIC 9(1).                                    
001300*                                 UNIT OF MEASURE                         
001400*                                 1 = 100'S, 0=UNITS                      
001500     03 VKARTNTO             PIC 9(7).                                    
001510*                                 NET WEIGHT IN GRAMS                     
001520     03 KDARTURS             PIC X(2).                                    
001530*                                 COUNTRY OF ORIGIN                       
001540     03 KVSEQ                PIC 9(5).                                    
001550*                                 SEQUENCE NUMBER                         
001560*                                 STARTS AT 1 PER INVOICE                 
001600*** END OF VILMAII-COPY LENGTH=                                           
