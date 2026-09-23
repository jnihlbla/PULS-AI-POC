000100 01  PRTY-W006PRTY.                                                       
000200*                                 STYRPARAMETRAR TILL                     
000300*                                 PRINTER SUBPROGRAM                      
000400     03 PRTY-IDPRTSPO        PIC X(8).                                    
000500*                                 SPOOL PRINTER SID-STORLEK               
000600*                                 SPOOL PRINTER PAGE SIZE                 
000700     03 PRTY-KDCOPIES        PIC X.                                       
000800*                                 ANTAL COPIOR VID PRINTNING              
000900*                                 NUMBER OF PRINTING COPIES               
001000     03 PRTY-KDFORMS         PIC X.                                       
001100*                                 KOD FÖR FORMSNUMMER                     
001200*                                 CODE FOR FORMSNUMBER                    
001300     03 PRTY-IDPFDEF         PIC X(8).                                    
001400*                                 IBM PSF FORMSDEF,PAGEDEF                
001500*                                 IBM PSF FORMSDEF,PAGEDEF                
001600     03 PRTY-IDCOPYG         PIC X(8).                                    
001700*                                 COPYGRUPP IBM PSF                       
001800*                                 COPYGROUP IBM PSF                       
001900     03 PRTY-FILLER          PIC X(24).                                   
002000     03 PRTY-IDTFX           PIC X(20).                                   
002100*                                 TELEFAXNUMMER                           
002200*                                 FAXNUMBER                               
002300     03 PRTY-TEFAX           OCCURS 5 TIMES                               
002400                             PIC X(50).                                   
002500*                                 FAX TEXTRAD TILL FÖRSÄTTSBLAD           
002600*                                 FAX INFO LINE                           
002700*** END OF VILMAII-COPY LENGTH= 320 BYTES                                 
