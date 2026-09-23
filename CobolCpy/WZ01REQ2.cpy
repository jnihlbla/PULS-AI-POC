000100 01  REQU-WZ01REQ2.                                                       
000200*                                 THE FIRST FIELDS IN AN REQUEST          
000300*                                 SENT FROM ONE SYSTEM COMPONENT          
000400*                                 TO ANOTHER.                             
000500*                                 !!! SECOND VERSION / IDREQVER =         
000600*                                  002 !!!                                
000700     03 REQU-IDREQVER        PIC 9(3).                                    
000800*                                 VERSION NUMBER OF REQUEST HEADE         
000900*                                 R MESSAGE                               
001000     03 REQU-IDRESVER        PIC 9(3).                                    
001100*                                 VERSION NUMBER OF RESPONSE HEAD         
001200*                                 ER MESSAGE                              
001300     03 REQU-IDINVER         PIC 9(3).                                    
001400*                                 VERSION NUMBER OF INPUT MESSAGE         
001500     03 REQU-IDOUTVER        PIC 9(3).                                    
001600*                                 VERSION NUMBER OF OUTPUT MESSAG         
001700*                                 E                                       
001800     03 REQU-KDPGMACT        PIC X.                                       
001900      88 REQU-QUERY          VALUE 'S'.                                   
002000      88 REQU-PREVIOUS       VALUE 'P'.                                   
002100      88 REQU-NEXT           VALUE 'N'.                                   
002200      88 REQU-FIRST          VALUE 'T'.                                   
002300      88 REQU-UPDATE         VALUE 'E'.                                   
002400      88 REQU-PRINT          VALUE 'H'.                                   
002500      88 REQU-UPD-V          VALUE 'V'.                                   
002600      88 REQU-UPD-X          VALUE 'X'.                                   
002700      88 REQU-UPD-Y          VALUE 'Y'.                                   
002800      88 REQU-RETURN         VALUE '3'.                                   
002900      88 REQU-SPLIT          VALUE '9'.                                   
003000*                                 TYPE OF PROGRAM ACTION                  
003100     03 REQU-IDUSER          PIC X(8).                                    
003200*                                 USER SECURITY-IDENTITY                  
003300     03 REQU-IDORIGSYS       PIC X(8).                                    
003400*                                 IDENTITY OF ORIGIN SYSTEM               
003500     03 REQU-IDCALLER        PIC X(8).                                    
003600*                                 IDENTITY OF TRANSACTION CALLER          
003700     03 REQU-IDAPI           PIC X(50).                                   
003800*                                 NAME OF THE API                         
003900     03 REQU-IDSERVICE       PIC X(50).                                   
004000*                                 NAME OF THE SERVICE                     
004100     03 REQU-AUTH-DATA.                                                   
004200        05 REQU-IDUSERKEY    PIC X(50).                                   
004300*                                 USER KEY                                
004400        05 REQU-IDCLIENTID   PIC X(15).                                   
004500*                                 CLIENT ID                               
004600        05 REQU-IDCLIENTSECRET                                            
004700                             PIC X(50).                                   
004800*                                 CLIENT SECRET                           
004900        05 REQU-IDJWT-ACCESS PIC X(3000).                                 
005000*                                 ACCESS TOKEN JSON WEB TOKEN             
005100*                                                                         
005200     03 REQU-FILLER          PIC X(500).                                  
005300*** END OF VILMAII-COPY LENGTH= 3752 BYTES                                
