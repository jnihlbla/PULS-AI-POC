000010*** EDIT ALLOWED                                                          
000100*    MSG-SPOOL-AREA.                                                      
000200*                                    * AREA FÖR SPOOL-API                 
000300 01  SPOOL-OPT-DEST          PIC X(8)    VALUE 'IAFPCC  '.                
000400                                                                          
000500 01  SPOOL-OPTIONS.                                                       
000600   03  SPOOL-START-OPTIONS.                                               
000700     05  SPOOL-OPT-LL        PIC S9(4)   VALUE +85       COMP.            
000800     05  FILLER              PIC S9(4)   VALUE ZERO      COMP.            
000900     05  SPOOL-IAFP          PIC X(14)   VALUE 'IAFP=A1M,PRTO='.          
001000   03  SPOOL-NORMAL-OPTIONS.                                              
001100     05  SPOOL-IAFP-LL       PIC S9(4)   VALUE +67       COMP.            
001200     05  FILLER              PIC X(6)    VALUE 'FORMS('.                  
001300     05  SPOOL-FORMS         PIC X(4)    VALUE 'STD '.                    
001400     05  FILLER              PIC X(9)    VALUE '),COPIES('.               
001500     05  SPOOL-COPIES        PIC X(1)    VALUE '1'.                       
001600     05  FILLER              PIC X(8)    VALUE '),CLASS('.                
001700     05  SPOOL-CLASS         PIC X(1)    VALUE 'A'.                       
001800     05  FILLER              PIC X(8)    VALUE '),LINECT'.                
001900     05  SPOOL-LINECT        PIC X(4)    VALUE '(00)'.                    
002000     05  FILLER              PIC X(5)    VALUE ',PRTY'.                   
002100     05  SPOOL-PRTY          PIC X(4)    VALUE '(12)'.                    
002200     05  FILLER              PIC X(6)    VALUE ',DEST('.                  
002300     05  SPOOL-IDNODE        PIC X(8)    VALUE SPACE.                     
002400     05  FILLER              PIC X(1)    VALUE ')'.                       
002500   03  SPOOL-OVR-PARAM.                                                   
002500     05  SPOOL-OVR-PARAM1    PIC X(19)   VALUE SPACE.                     
002500     05  SPOOL-OVR-PARAM2    PIC X(150)  VALUE SPACE.                     
002600                                                                          
002610         EJECT                                                            
002700 01  SPOOL-IBM-LASER.                                                     
002710   03  SPOOL-IBM-LASER-DEL1.                                              
002800     05  FILLER              PIC X(9)    VALUE ',FORMDEF('.               
002900     05  SPOOL-FORMDEF       PIC X(6)    VALUE SPACE.                     
003000     05  FILLER              PIC X(10)   VALUE '),PAGEDEF('.              
003100     05  SPOOL-PAGEDEF       PIC X(6)    VALUE SPACE.                     
003200     05  FILLER              PIC X(9)    VALUE '),PRMODE('.               
003300     05  SPOOL-PRMODE        PIC X(5)    VALUE 'SOSI1'.                   
003400     05  FILLER              PIC X(14)   VALUE '),PIMSG(YES,8)'.          
003500     05  FILLER              PIC X(9)    VALUE ',USERLIB('.               
003700   03  SPOOL-USERLIB         PIC X(30)   VALUE 'WV2.PROD.PSF)'.           
003800                                                                          
003810 01  SPOOL-USERLIB-LENGTH    PIC S9(4)   VALUE +13       COMP.            
003820                                                                          
004000 01  SPOOL-FEEDBACK.                                                      
004100   03  FILLER                PIC S9(4)   VALUE +244      COMP.            
004200   03  FILLER                PIC S9(4)   VALUE ZERO      COMP.            
004300   03  FILLER                PIC X(240)  VALUE SPACE.                     
004400                                                                          
004410         EJECT                                                            
004500 01  SPOOL-RAD-AREA.                                                      
004600   03  SPOOL-RAD-BDW         PIC S9(4)   VALUE +141      COMP.            
004700   03  FILLER                PIC S9(4)   VALUE ZERO      COMP.            
004800   03  SPOOL-RADER.                                                       
006200     05  FILLER              PIC X(5).                                    
006300     05  SPOOL-DATA          PIC X(9585).                                 
004900   03  FILLER REDEFINES SPOOL-RADER OCCURS 70 TIMES.                      
005000     05  SPOOL-RAD-RDW       PIC S9(4)                   COMP.            
005100     05  SPOOL-RAD-ZZ        PIC S9(4)                   COMP.            
005200     05  SPOOL-RAD.                                                       
005300       07  SPOOL-RAD-STYR    PIC X(1).                                    
005400       07  SPOOL-RAD-DATA    PIC X(132).                                  
005500   03  FILLER REDEFINES SPOOL-RADER OCCURS 80 TIMES.                      
005600     05  SPOOL-A4S-RDW       PIC S9(4)                   COMP.            
005700     05  SPOOL-A4S-ZZ        PIC S9(4)                   COMP.            
005800     05  SPOOL-A4S.                                                       
005900       07  SPOOL-A4S-STYR    PIC X(1).                                    
006000       07  SPOOL-A4S-DATA    PIC X(80).                                   
004700   03  FILLER                PIC X(6790) VALUE LOW-VALUE.                 
006310                                                                          
007000 01  SPOOL-FAX-AREA.                                                      
007100   03  SPOOL-FAX-BDW         PIC S9(4)   VALUE +589      COMP.            
007200   03  FILLER                PIC S9(4)   VALUE ZERO      COMP.            
007400   03  SPOOL-FAX OCCURS 9 TIMES.                                          
007500     05  SPOOL-FAX-RDW       PIC S9(4)                   COMP.            
007600     05  SPOOL-FAX-ZZ        PIC S9(4)                   COMP.            
007700     05  SPOOL-FAX.                                                       
007800       07  SPOOL-FAX-STYR    PIC X(1).                                    
007900       07  SPOOL-FAX-DATA    PIC X(60).                                   
