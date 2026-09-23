000100 01  FPI-WDD101.                                                          
000200*                                 FÖRPACKNINGSINSTRUKTION                 
000300*                                                                         
000400*                                 FYSISK NYCKEL: IDFPINST                 
000500     03 FPI-IDFPINST         PIC S9(7)           COMP-3.                  
000600*                                 FÖRPACKNINGSINSTRUKTION NR              
000700*                                 PACKAGE INSTRUCTION NUMBER              
000800     03 FPI-IDLEVNR          PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001100     03 FPI-TEFPINST         OCCURS 10 TIMES                              
001200                             PIC X(80).                                   
001300*                                 TEXT FÖRPACKNINGSINSTRUKTION            
001400     03 FPI-DAREGDAT         PIC 9(8).                                    
001500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001600*                                 REGISTRATION DATE (YYYYMMDD)            
001700     03 FPI-IDUSER           PIC X(8).                                    
001800*                                 ANVÄNDARENS SÄKERHETS ID                
001900*                                 USER SECURITY-IDENTITY                  
002000     03 FPI-TIUPPDAT         PIC S9(7)           COMP-3.                  
002100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002200*                                 UPDATING DATE     (YYMMDD)              
002300     03 FILLER               PIC X(11).                                   
002400*** END OF VILMAII-COPY LENGTH= 840 BYTES                                 
