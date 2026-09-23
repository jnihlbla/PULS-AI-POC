000100 01  OUTP-WZ11OUT.                                                        
000200*                                 PARAMETERS TO THE GENERAL SUB           
000300*                                 PROGRAM WZ11OUTP (PRINT)                
000400*                                 CALLS:                                  
000500*                                    USING OUTP-WZ11OUT                   
000600*                                                                         
000700*                                 IDCALL=001-9 DEPENDING ON WHICH         
000800*                                  PRINT "CHANNEL" IS USED.               
000900*                                  (NORMALLY PCB NAME "OUT*" IS           
001000*                                  USED, BUT AN EXPLICIT PCB NAME         
001100*                                  MAY ALSO BE SPECIFIED IN THE.          
001200*                                  OPEN CALL.)                            
001300*                                 KDFUNC="OPEN", "PUT" OR "CLOSE"         
001400*                                 KDRC=0 IF OK; = 8 IF OPEN FAILS         
001500*                                                                         
001600     03 OUTP-IDCALL          PIC S9(9)           COMP.                    
001700*                                 ID OF CALL SEQUENCE                     
001800     03 OUTP-KDFUNC          PIC X(10).                                   
001900*                                 FUNCTION CODE                           
002000     03 OUTP-KDRC            PIC S9(9)           COMP.                    
002100*                                 RETURN CODE                             
002200     03 OUTP-OPEN-PARMS-FILLER.                                           
002300        05 OUTP-OPEN-PARMS.                                               
002400*                                 FIELDS USED ON OPEN CALL.               
002500           07 OUTP-IDOUTDEST PIC X(60).                                   
002600*                                 PHYSICAL OUTPUT DESTINATION             
002700           07 OUTP-KVCOPIES  PIC X.                                       
002800*                                 NUMBER OF PRINTED COPIES                
002900           07 OUTP-IDPFDEF   PIC X(8).                                    
003000*                                 IBM PSF FORMSDEF,PAGEDEF                
003100           07 OUTP-IDFORMSNM PIC X(8).                                    
003200*                                 FORMS NAME                              
003300           07 OUTP-FLCARRCNTL                                             
003400                             PIC X.                                       
003500*                                 IS CARRRIAGE CONTROL INCLUDED?          
003600           07 OUTP-FLACIF    PIC X.                                       
003700*                                 SHOULD ACIF BE USED?                    
003800           07 OUTP-IDPCB     PIC X(8).                                    
003900*                                 PCB NAME                                
004000           07 OUTP-IDOUTTYPE PIC X(15).                                   
004100*                                 OUTPUT TYPE                             
004200           07 OUTP-IDOUTREC  PIC X(30).                                   
004300*                                 OUTPUT RECEIVER                         
004400           07 OUTP-IDLIST    PIC X(10).                                   
004500*                                 LIST IDENTITY                           
004600           07 OUTP-TIREGDAT  PIC 9(6).                                    
004700*                                 REGISTRATION DATE (YYMMDD)              
004800           07 OUTP-TIKLOCK   PIC 9(8).                                    
004900*                                 TIME OF DAY (HHMMSSTH)                  
005000        05 FILLER            PIC X(2848).                                 
005100     03 OUTP-PUT-PARAMETERS REDEFINES OUTP-OPEN-PARMS-FILLER.             
005200*                                 FIELDS USED ON PUT CALL                 
005300        05 OUTP-TEOUTDATA-L  PIC S9(9)           COMP.                    
005400*                                 LENGTH OF DATA                          
005500        05 OUTP-TEOUTDATA    PIC X(3000).                                 
005600*                                 OUTPUT DATA                             
005700*** END OF VILMAII-COPY LENGTH= 3022 BYTES                                
