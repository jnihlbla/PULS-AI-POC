000100 01  REQU-WZ01REQU.                                                       
000200*                                 THE FIRST FIELDS IN AN REQUEST          
000300*                                 SENT FROM ONE SYSTEM COMPONENT          
000400*                                 TO ANOTHER.                             
000500     03 REQU-IDMSGVER        PIC 9(3).                                    
000600*                                 VERSIONSNUMMER PÅ MEDDELANDE            
000700*                                 VERSION NUMBER OF THE MESSAGE           
000800     03 REQU-KDPGMACT        PIC X.                                       
000900      88 REQU-QUERY          VALUE 'S'.                                   
001000      88 REQU-PREVIOUS       VALUE 'P'.                                   
001100      88 REQU-NEXT           VALUE 'N'.                                   
001200      88 REQU-FIRST          VALUE 'T'.                                   
001300      88 REQU-UPDATE         VALUE 'E'.                                   
001400      88 REQU-PRINT          VALUE 'H'.                                   
001500      88 REQU-UPD-V          VALUE 'V'.                                   
001600      88 REQU-UPD-X          VALUE 'X'.                                   
001700      88 REQU-UPD-Y          VALUE 'Y'.                                   
001800      88 REQU-RETURN         VALUE '3'.                                   
001900      88 REQU-SPLIT          VALUE '9'.                                   
002000*                                 TYP AV PROGRAMBEARBETNING               
002100*                                 TYPE OF PROGRAM ACTION                  
002200     03 REQU-IDUSER          PIC X(8).                                    
002300*                                 ANVÄNDARENS SÄKERHETS ID                
002400*                                 USER SECURITY-IDENTITY                  
002500*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
