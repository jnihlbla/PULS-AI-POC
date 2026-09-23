000100 01  W080R88A.                                                            
000200*                                 COPYTEXT FÖR DATA FRÅN REBUS TI         
000300*                                 LL PARTS KUNDREGISTER                   
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDCPYTXT             PIC X(8).                                    
000700*                                 COPYTEXT IDENTITET                      
000800     03 IDSNDNOD             PIC X(8).                                    
000900*                                 SÄNDANDE NODE IDENTITET                 
001000     03 IDSNDJOB             PIC X(8).                                    
001100*                                 SÄNDANDE JOB IDENTITET                  
001200     03 IDBET.                                                            
001300*                                 IDENTITET JURIDISK KUND/BETALAR         
001400*                                 E                                       
001500        05 IDFTG             PIC 9(2).                                    
001600*                                 FÖRETAGSID EKONOM REDOVISNING           
001700        05 IDLANDX2          PIC X(2).                                    
001800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001900        05 IDBETNR           PIC X(5).                                    
002000*                                 LÖPNUMMER BETALARE                      
002100     03 BEBET.                                                            
002200*                                 BETALNINGSANSVARIG NAMN                 
002300        05 BEBETRAD-1        PIC X(35).                                   
002400*                                 DEL AV BETALAR/KUND NAMN                
002500        05 BEBETRAD-2        PIC X(35).                                   
002600*                                 DEL AV BETALAR/KUND NAMN                
002700     03 ADBET.                                                            
002800*                                 BETALNINGSANSVARIG ADRESS               
002900        05 ADBETRAD-1        PIC X(35).                                   
003000*                                 ADRESSRAD BETALARE/KUND                 
003100        05 ADBETRAD-2        PIC X(35).                                   
003200*                                 ADRESSRAD BETALARE/KUND                 
003300     03 IDTFN                PIC X(20).                                   
003400*                                 TELEFONNUMMER                           
003500     03 IDTLX                PIC X(20).                                   
003600*                                 TELEXNUMMER                             
003700     03 IDTFX                PIC X(20).                                   
003800*                                 TELEFAXNUMMER                           
003900     03 IDTLX-MEMO           PIC X(20).                                   
004000*                                 TELEXNUMMER MEMO                        
004100     03 IDTFX-MEMO           PIC X(20).                                   
004200*                                 MEMOFAXNUMMER                           
004300     03 KDBETTYP             PIC X(3).                                    
004400*                                 TYP AV BETALARE                         
004500     03 TISTADAT             PIC S9(7)           COMP-3.                  
004600*                                 GENERELLT STARTDATUM                    
004700     03 TIBETUPH             PIC S9(7)           COMP-3.                  
004800*                                 KUND/BETALARE UPPHÖR DATUM              
004900     03 KDKREDSP             PIC X.                                       
005000*                                 KREDITSPÄRR/VARNING PÅ KUND             
005100     03 TEBETINF             PIC X(30).                                   
005200*                                 INFORMATIONSTEXT KUND                   
005300     03 KDBETVIL-H           PIC X(3).                                    
005400*                                 HUVUDBETALNINGSVILLKOR                  
005500     03 KDBETVIL-ALT1        PIC X(3).                                    
005600*                                 BETALNINGSVILLKOR ALTERNATIV 1          
005700     03 KDBETVIL-ALT2        PIC X(3).                                    
005800*                                 BETALNINGSVILLKOR ALT 2                 
005900     03 KDBETVIL-ALT3        PIC X(3).                                    
006000*                                 BETALNINGSVILLKOR ALT 3                 
006100     03 KDBETVIL-ALT4        PIC X(3).                                    
006200*                                 BETALNINGSVILLKOR ALT 4                 
006300     03 KDVALREB-H.                                                       
006400*                                 KUNDENS HUVUDVALUTA                     
006500        05 KDVALISO-H        PIC X(3).                                    
006600*                                 HUVUDVALUTA                             
006700        05 KDVALKT-H         PIC X.                                       
006800*                                 KURSTYP HUVUDVALUTA                     
006900     03 KDVALREB-ALT1.                                                    
007000*                                 ALTERNATIV VALUTAKOD 1 REBUS            
007100        05 KDVALISO-ALT1     PIC X(3).                                    
007200*                                 VALUTAKOD ALT 1                         
007300        05 KDVALKT-ALT1      PIC X.                                       
007400*                                 KURSTYP ALT 1                           
007500     03 KDVALREB-ALT2.                                                    
007600*                                 ALTERNATIV VALUTAKOD 2 REBUS            
007700        05 KDVALISO-ALT2     PIC X(3).                                    
007800*                                 VALUTAKOD ALT 2                         
007900        05 KDVALKT-ALT2      PIC X.                                       
008000*                                 KURSTYP ALT 2                           
008100     03 KDVALREB-ALT3.                                                    
008200*                                 ALTERNATIV VALUTAKOD 3 REBUS            
008300        05 KDVALISO-ALT3     PIC X(3).                                    
008400*                                 VALUTAKOD ALT 3                         
008500        05 KDVALKT-ALT3      PIC X.                                       
008600*                                 KURSTYP ALT 3                           
008700     03 KDVALREB-ALT4.                                                    
008800*                                 ALTERNATIV VALUTAKOD 4 REBUS            
008900        05 KDVALISO-ALT4     PIC X(3).                                    
009000*                                 VALUTAKOD ALT 4                         
009100        05 KDVALKT-ALT4      PIC X.                                       
009200*                                 KURSTYP ALT 4                           
009300*** END COPY W080R88AC0  LENGTH=353                                       
