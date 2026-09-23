000100 01  REQU-W90515I1.                                                       
000200*                                 REQUEST TO PGM W90515                   
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
006000        05 FILLER            PIC X(500).                                  
006100     03 REQU-IDARTNR-KEY     PIC 9(9).                                    
006200*                                 ARTIKELNUMMER                           
006300     03 REQU-FLANSK          PIC X.                                       
006400*                                 ALLMÄN FLAGGA                           
006500     03 REQU-FLLOGG          PIC X.                                       
006600*                                 ALLMÄN FLAGGA                           
006700     03 REQU-IDSKYLT-KEY     PIC X(3).                                    
006800      88 REQU-GODK-IDSKYLT   VALUE 'CZ '                                  
006900                             'D  '                                        
007000                             'DK '                                        
007100                             'E  '                                        
007200                             'FB '                                        
007300                             'GB '                                        
007400                             'GR '                                        
007500                             'H  '                                        
007600                             'I  '                                        
007700                             'IR '                                        
007800                             'J  '                                        
007900                             'KOR'                                        
008000                             'MAL'                                        
008100                             'NL '                                        
008200                             'P  '                                        
008300                             'PL '                                        
008400                             'RC '                                        
008500                             'RCN'                                        
008600                             'RO '                                        
008700                             'RUS'                                        
008800                             'S  '                                        
008900                             'SF '                                        
009000                             'T  '                                        
009100                             'TR '                                        
009200                             'USA'                                        
009300                             'YU '.                                       
009400*                                 NATIONALITETSTECKEN                     
009500*                                 SPRÅKIDENTIFIKATION                     
009600     03 REQU-IDDOKTYP-KEY    PIC X(8).                                    
009700*                                 DOKUMENTATIONSTYP                       
009800     03 REQU-IDDOK-KEY       PIC X(8).                                    
009900*                                 DOKUMENTATIONSIDENTITET                 
010000     03 REQU-TEINFO          PIC X(1200).                                 
010100*                                 ALLMÄN TEXT INFO                        
010200*** END OF VILMAII-COPY LENGTH= 4982 BYTES                                
