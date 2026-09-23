000100 01  W479971.                                                             
000200*                                 CTXT FÖR SKAPANDE AV PREEXTRAKT         
000300*                                 FÖR WDQ2; SEG WDQ201.                   
000400     03 IDSEGM               PIC X(6).                                    
000500*                                 SEGMENT                                 
000600     03 IDORDER              PIC S9(7)           COMP-3.                  
000700*                                 VOLVO PARTS ORDERNUMMER                 
000800     03 FLKLAR               PIC X.                                       
000900*                                 AVSLUTNINGSMARKERING                    
001000     03 KDORDKL              PIC S9              COMP-3.                  
001100*                                 ORDERKLASS                              
001200     03 IDUSER               PIC X(8).                                    
001300*                                 ANVÄNDARENS SÄKERHETS ID                
001400     03 IDSYSTEM             PIC X(4).                                    
001500*                                 VOLVO VCCS SYSTEMNUMMER                 
001600     03 TIREPDAT             PIC S9(7)           COMP-3.                  
001700*                                 REPAIR DATE                             
001800*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
