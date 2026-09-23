000100 01  W37167.                                                              
000200*                                 COPYTEXT F÷R FILEN W37167               
000300*                                                                         
000400     03 SOR0-IDDISTR         PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 SOR0-IDKUNDNR        PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 SOR0-IDRONR          PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 SOR0-TIRODAT         PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (≈≈MMDD)         
001200     03 SOR0-IDPTYP          PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 SOR0-IDLOPNR         PIC S9(5)           COMP-3.                  
001500*                                 L÷PNUMMER          IDLOPNR-002          
001600     03 IDPTYP               PIC X(3).                                    
001700*                                 POSTTYP                                 
001800     03 IDARTNR              PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000     03 KVPOINT              PIC S9(7)           COMP-3.                  
002100*                                 POINT VALUE                             
002200     03 KDEXCHA              PIC S9(3)           COMP-3.                  
002300*                                 EXCHANGE ACCOUNT CODE                   
002400     03 FILLER               PIC X(45).                                   
002500*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
