000100 01  W47841.                                                              
000200*                                 DISTRIBUTION FOLLOW UP PER              
000300*                                 DC, RFSDATE, TRANSPORT.                 
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 TIRFSDAT             PIC 9(6).                                    
000900*                                 KLART F÷R TRANSPORT ≈≈MMDD              
001000*                                 READY FOR SHIPMENT  YYMMDD              
001100     03 IDTRP.                                                            
001200*                                 TRANSPORTIDENTITET                      
001300*                                 TRANSPORTIDENTITY                       
001400        05 IDTRPLOS          PIC X(3).                                    
001500*                                 TRANSPORTL÷SNING                        
001600*                                 TRANSPORTSOLUTION                       
001700        05 IDTRPVAR          PIC X(2).                                    
001800*                                 TRANSPORTL÷SNINGSGRUPP                  
001900*                                 TRANSPORTSOLUTIONGROUP                  
002000     03 IDPRODNR             PIC S9(7)           COMP-3.                  
002100*                                 PRODUKTIONSNUMMER                       
002200*                                 PRODUCTION NUMBER                       
002300     03 KDORDKL              PIC S9              COMP-3.                  
002400*                                 ORDERKLASS                              
002500*                                 ORDER CLASS                             
002600     03 KVORDER              PIC S9(7)           COMP-3.                  
002700*                                 ANTAL ORDER                             
002800*                                 QUANTITY OF ORDERS                      
002900     03 KVORDER-UTSKR        PIC S9(7)           COMP-3.                  
003000*                                 ANTAL ORDER                             
003100*                                 QUANTITY OF ORDERS                      
003200     03 KVORDER-PACK         PIC S9(7)           COMP-3.                  
003300*                                 ANTAL PACKADE ORDER                     
003400*                                 QUANTITY OF PACKED ORDERS               
003500     03 KVORDRAD             PIC S9(5)           COMP-3.                  
003600*                                 ANTAL ORDERRADER                        
003700*                                 NUMBER OF ORDER LINES                   
003800     03 KVORDRAD-UTSKR       PIC S9(5)           COMP-3.                  
003900*                                 ANTAL UTSKR ORDERRAD                    
004000     03 KVORDRAD-PACK        PIC S9(5)           COMP-3.                  
004100*                                 ANTAL PACKADE ORDERRADER                
004200     03 KVKOLLI              PIC S9(5)           COMP-3.                  
004300*                                 ANTAL KOLLI                             
004400*                                 NBR OF CASES                            
004500     03 KVKOLLI-LAST         PIC S9(5)           COMP-3.                  
004600*                                 ANTAL LASTNINGSRAPPORTERADE             
004700*                                 KOLLIN                                  
004800     03 KVKOLLI-FAKT         PIC S9(5)           COMP-3.                  
004900*                                 ANTAL FAKTURERADE KOLLIN                
005000*                                 QUANTITY CASES INVOICED                 
005100*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
