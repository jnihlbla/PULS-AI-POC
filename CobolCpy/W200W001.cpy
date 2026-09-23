000010*** EDIT ALLOWED                                                          
000100 01  W200W001.                                                            
000200*             *** UPPLADDNINGSFAKTOR FÖR PERIOD 5        *****            
000300*             *** PER IDPROD                             *****            
000400     03  MAXINDX-1           PIC S9(3)  COMP-3  VALUE +9.                 
000500*                                                                         
000600     03  VARDEN.                                                          
000700*****************************************************************         
000800       05  IDPROD-01-RDEL    PIC X(6)        VALUE '017087'.              
000900       05  IDPROD-02-DRIVL   PIC X(6)        VALUE '020090'.              
001000       05  IDPROD-03-CHASSI  PIC X(6)        VALUE '020090'.              
001100       05  IDPROD-04-BYTES   PIC X(6)        VALUE '020090'.              
001200       05  IDPROD-05-TILLB   PIC X(6)        VALUE '014084'.              
001300       05  IDPROD-06-RADIO   PIC X(6)        VALUE '014084'.              
001400       05  IDPROD-07-TELE    PIC X(6)        VALUE '014084'.              
001500       05  IDPROD-08-VERKST  PIC X(6)        VALUE '014084'.              
001600       05  IDPROD-09-EMB     PIC X(6)        VALUE '014084'.              
001700******************************************************************        
001800     03  TABW200   REDEFINES VARDEN.                                      
001900      05  INDX-1   OCCURS 9.                                              
002000*                   ***                                                   
002100          07  REPER5             DISPLAY PIC 99V9.                        
002200          07  REAAR              DISPLAY PIC 99V9.                        
002300*                            ***                                          
002400*** END COPY W200W001C0  LENGTH=56    OLD LENGTH=56                       
