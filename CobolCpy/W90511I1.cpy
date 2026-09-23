000100 01  REQU-W90511I1.                                                       
000200*                                 REQUEST TO PGM W90511                   
000300     03 REQU-WZ01REQ2.                                                    
000400*                                 THE FIRST FIELDS IN AN REQUEST          
000500*                                 SENT FROM ONE SYSTEM COMPONENT          
000600*                                 TO ANOTHER.                             
000700*                                 !!! SECOND VERSION / IDREQVER =         
000800*                                  002 !!!                                
000900        05 REQU-IDREQVER     PIC 9(3).                                    
001000*                                 VERSIONSNUMMER FÖR MEDDELANDE O         
001100*                                 M BEGÄRANDEHUVUD                        
001200        05 REQU-IDRESVER     PIC 9(3).                                    
001300*                                 VERSIONSNUMMER FÖR MEDDELANDE O         
001400*                                 M SVARSHUVUD                            
001500        05 REQU-IDINVER      PIC 9(3).                                    
001600*                                 VERSIONSNUMMER PÅ INDATAMEDDELA         
001700*                                 NDE                                     
001800        05 REQU-IDOUTVER     PIC 9(3).                                    
001900*                                 VERSIONSNUMMER PÅ UTDATAMEDDELA         
002000*                                 NDE                                     
002100        05 REQU-KDPGMACT     PIC X.                                       
002200         88 REQU-QUERY       VALUE 'S'.                                   
002300         88 REQU-PREVIOUS    VALUE 'P'.                                   
002400         88 REQU-NEXT        VALUE 'N'.                                   
002500         88 REQU-FIRST       VALUE 'T'.                                   
002600         88 REQU-UPDATE      VALUE 'E'.                                   
002700         88 REQU-PRINT       VALUE 'H'.                                   
002800         88 REQU-UPD-V       VALUE 'V'.                                   
002900         88 REQU-UPD-X       VALUE 'X'.                                   
003000         88 REQU-UPD-Y       VALUE 'Y'.                                   
003100         88 REQU-RETURN      VALUE '3'.                                   
003200         88 REQU-SPLIT       VALUE '9'.                                   
003300*                                 TYP AV PROGRAMBEARBETNING               
003400        05 REQU-IDUSER       PIC X(8).                                    
003500*                                 ANVÄNDARENS SÄKERHETS ID                
003600        05 REQU-IDORIGSYS    PIC X(8).                                    
003700*                                 SYSTEMET MED URSPRUNGETS IDENTI         
003800*                                 TET                                     
003900        05 REQU-IDCALLER     PIC X(8).                                    
004000*                                 TRANSAKTIONSANROPARENS IDENTITE         
004100*                                 T                                       
004200        05 REQU-IDAPI        PIC X(50).                                   
004300*                                 NAMN PÅ API                             
004400        05 REQU-IDSERVICE    PIC X(50).                                   
004500*                                 NAMN PÅ SERVICE                         
004600        05 REQU-AUTH-DATA.                                                
004700*                                                                         
004800           07 REQU-IDUSERKEY PIC X(50).                                   
004900*                                 USER KEY                                
005000           07 REQU-IDCLIENTID                                             
005100                             PIC X(15).                                   
005200*                                 CLIENT ID                               
005300           07 REQU-IDCLIENTSECRET                                         
005400                             PIC X(50).                                   
005500*                                 CLIENT SECRET                           
005600           07 REQU-IDJWT-ACCESS                                           
005700                             PIC X(3000).                                 
005800*                                 ACCESS TOKEN JSON WEB TOKEN             
005900*                                                                         
006000        05 REQU-FILLER       PIC X(500).                                  
006100     03 REQU-IDARTNR-KEY     PIC 9(9).                                    
006200*                                 ARTIKELNUMMER                           
006300     03 REQU-IDDC-KEY-MIN    PIC X(2).                                    
006400*                                 IDENTIFIERARE LAGER                     
006500     03 REQU-IDDC-KEY-MAX    PIC X(2).                                    
006600*                                 IDENTIFIERARE LAGER                     
006700*** END OF VILMAII-COPY LENGTH= 3765 BYTES                                
