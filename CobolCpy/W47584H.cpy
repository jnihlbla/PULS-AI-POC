000010*** EDIT ALLOWED                                                          
000100 01  W47584H.                                                             
000200*                                 NORTH AMERICAN BROKERS                  
000210*                                 INFORMATION: HEADER                     
000300*                                                                         
000400     03 IDPTYP               PIC X(1).                                    
000500*                                 RECORD IDENTIFIER 'H'                   
000600     03 IDFAKT               PIC X(10).                                   
000700*                                 INVOICE NUMBER                          
000710     03 IDDISTR              PIC 9(4).                                    
000720*                                 DISTRICT NUMBER                         
000730     03 IDKUNDNR             PIC 9(6).                                    
000740*                                 RETAILER NUMBER                         
000800     03 DAFAKT               PIC 9(8).                                    
000900*                                 INVOICE DATE YYYYMMDD                   
000910     03 KDFAKT               PIC 9(1).                                    
000920*                                 INVOICE TYPE '1' REGULAR                
000930*                              (FLOVRLEV=JA)   '2' DISCREPANCY            
001000     03 BEBROKER             PIC X(40).                                   
001100*                                 BROKER NAME                             
001510     03 KDFRAKT              PIC 9(2).                                    
001520*                                 FREIGHT CODE (TRANSPORT MODE)           
001530     03 BESLULEV             PIC X(20).                                   
001540*                                 CONTAINER NUMBER                        
001550     03 BECITY               PIC X(20).                                   
001560*                  (GOTHENBURG)   CITY (DELIVERY ORIGIN)                  
001570     03 IDSKEPPN             PIC 9(7).                                    
001580*                                 SHIPMENT NUMBER                         
001590     03 IDBOKN               PIC X(15).                                   
001591*                                 BOOKING NUMBER                          
001592     03 VKORDBTO-FAKT        PIC 9(7)V9(1).                               
001593*                                 GROSS WEIGHT                            
001594     03 VKORDNTO-FAKT        PIC 9(7)V9(1).                               
001595*                                 NET   WEIGHT                            
001596     03 BEWEIGHT             PIC X(1).                                    
001597*                                 WEIGHT QUALIFIER  'M' = METRIC          
001598*                                                    U' = US              
001599     03 VLORDBTO-FAKT        PIC 9(5)V9(3).                               
001600*                                 GROSS VOLUME                            
001601     03 BEVOLUME             PIC X(1).                                    
001610*                                 VOLUME QUALIFIER  'M' 0 METRIC          
001620*                                                    U' = US              
001622     03 KDVALISO-SEK         PIC X(3).                                    
001623*                                 CURRENCY CODE SEK                       
001630     03 SUORDV-FAKT          PIC 9(9)V9(2).                               
001640*                                 GOODS VALUE IN SEK                      
001670     03 PREMBHNT             PIC 9(7)V9(2).                               
001680*                                 PACKING AND HANDLING IN SEK             
001681     03 PRFRAKT              PIC 9(7)V9(2).                               
001682*                                 FREIGHT COST IN SEK                     
001690     03 SUFKTBEL             PIC 9(9)V9(2).                               
001691*                                 TOTAL VALUE                             
001692     03 KDVALISO-UTL         PIC X(3).                                    
001693*                                 CURRENCY CODE LOCAL                     
001694     03 PRKURS               PIC 9(6)V9(5).                               
001695*                                 RATE OF EXCHANGE                        
001696     03 SUFKTUTL             PIC 9(11)V9(2).                              
001697*                                 TOTAL VALUE LOCAL CURRENCY              
001696     03 PRFOERS              PIC 9(7)V9(2).                               
001697*                                 INSURANCE COST IN SEK                   
001700*** END OF VILMAII-COPY LENGTH=                                           
