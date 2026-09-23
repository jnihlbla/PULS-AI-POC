000100* GENERATION OF COBOL HOST STRUCTURE FROM T01NSDO-TAB                     
000200  01 T01NSDO.                                                             
000300*              T01NSDO                                                    
000400   03 IDLEGSEL         PIC X(4).                                          
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 IDLOPNR          PIC S9(3) COMP-3.                                  
000700*              LÖPNUMMER                                                  
000800   03 BETEXT           PIC X(55).                                         
000900   03 IDFINDOC-START   PIC S9(9) COMP-3.                                  
001000*              FINANSIELLT DOKUMENT START                                 
001100   03 IDFINDOC-NEXT    PIC S9(9) COMP-3.                                  
001200*              FINANSIELLT DOKUMENT NÄSTA                                 
001300   03 IDFINDOC-STOP    PIC S9(9) COMP-3.                                  
001400*              FINANSIELLT DOKUMENT STOPP                                 
001500   03 DAREGDAT         PIC X(8).                                          
001600*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
001700   03 DAUPPDAT         PIC X(8).                                          
001800*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
001900   03 IDUSER           PIC X(8).                                          
002000*              ANVÄNDARENS SÄKERHETS ID                                   
002100*                                                                         
002200*** END OF VILMAII-COPY LENGTH= 100 OLD LENGTH=                           
