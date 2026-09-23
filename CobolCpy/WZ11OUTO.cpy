000100 01  OUTO-WZ11OUT.                                                        
000200*                                 PARAMETERS TO THE GENERAL SUB           
000300*                                 PROGRAM WZ11OUTO (ON-DEMAND)            
000400*                                 CALLS:                                  
000500*                                    USING OUTO-WZ11OUT                   
000600*                                                                         
000700*                                 IDCALL=001-9 DEPENDING ON WHICH         
000800*                                  OUTPUT "CHANNEL" IS USED.              
000900*                                  (NORMALLY PCB NAME "OUT*" IS           
001000*                                  USED, BUT AN EXPLICIT PCB NAME         
001100*                                  MAY ALSO BE SPECIFIED IN THE.          
001200*                                  OPEN CALL.)                            
001300*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
001400*                                 KDRC=0 IF OK; = 8 IF OPEN FAILS         
001500*                                                                         
001600     03 OUTO-IDCALL          PIC S9(9)           COMP.                    
001700*                                 ID OF CALL SEQUENCE                     
001800     03 OUTO-KDFUNC          PIC X(10).                                   
001900*                                 FUNCTION CODE                           
002000     03 OUTO-KDRC            PIC S9(9)           COMP.                    
002100*                                 RETURN CODE                             
002200     03 OUTO-OPEN-PARMS-FILLER.                                           
002300        05 OUTO-OPEN-PARMS.                                               
002400*                                 FIELDS USED ON OPEN CALL.               
002500*                                 NORMALLY FORMS NAME CONTROLS            
002600*                                 WHICH FORM IS USED, BUT FOR             
002700*                                 ACIF PAGE/FORMDEF SHOULD ALSO           
002800*                                 BE SPECIFIED.                           
002900           07 OUTO-IDPFDEF   PIC X(8).                                    
003000*                                 IBM PSF FORMSDEF,PAGEDEF                
003100           07 OUTO-IDFORMSNM PIC X(8).                                    
003200*                                 FORMS NAME                              
003300           07 OUTO-FLCARRCNTL                                             
003400                             PIC X.                                       
003500*                                 IS CARRRIAGE CONTROL INCLUDED?          
003600           07 OUTO-FLACIF    PIC X.                                       
003700*                                 SHOULD ACIF BE USED?                    
003800           07 OUTO-IDPCB     PIC X(8).                                    
003900*                                 PCB NAME                                
004000           07 OUTO-IDOUTTYPE PIC X(15).                                   
004100*                                 OUTPUT TYPE                             
004200           07 OUTO-IDOUTREC  PIC X(30).                                   
004300*                                 OUTPUT RECEIVER                         
004400           07 OUTO-IDLIST    PIC X(10).                                   
004500*                                 LIST IDENTITY                           
004600           07 OUTO-TIREGDAT  PIC 9(6).                                    
004700*                                 REGISTRATION DATE (YYMMDD)              
004800           07 OUTO-TIKLOCK   PIC 9(8).                                    
004900*                                 TIME OF DAY (HHMMSSTH)                  
005000           07 OUTO-IDOUTDEST PIC X(60).                                   
005100*                                 PHYSICAL OUTPUT DESTINATION             
005200        05 FILLER            PIC X(2849).                                 
005300     03 OUTO-PUT-PARAMETERS REDEFINES OUTO-OPEN-PARMS-FILLER.             
005400*                                 FIELDS USED ON PUT CALL                 
005500        05 OUTO-TEOUTDATA-L  PIC S9(9)           COMP.                    
005600*                                 LENGTH OF DATA                          
005700        05 OUTO-TEOUTDATA    PIC X(3000).                                 
005800*                                 OUTPUT DATA                             
005900*** END OF VILMAII-COPY LENGTH= 3022 BYTES                                
