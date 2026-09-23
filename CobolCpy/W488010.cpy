000100 01  SALDO-W488010.                                                       
000200*                                 SALDOFÖRÄNDRINGAR FRÅN PDP              
000300*                                 PER ARTIKEL                             
000400     03 FILLER               PIC X.                                       
000500     03 SALDO-IDPTYP-010     PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 FILLER               PIC X(4).                                    
000800     03 SALDO-IDARTNR        PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 FILLER               PIC X.                                       
001100     03 SALDO-KDTECKEN-BUFF-F                                             
001200                             PIC X.                                       
001300      88 SALDO-KDTECKEN-PLUS VALUE '+'                                    
001400                             ' '.                                         
001500      88 SALDO-KDTECKEN-MINUS                                             
001600                             VALUE '-'.                                   
001700*                                 PLUS, MINUSTECKEN ALT ASTERISK          
001800     03 FILLER               PIC X(2).                                    
001900     03 SALDO-KVBUFF-F       PIC X(7).                                    
002000*                                 FÖRÄDLAT BUFFERSALDO                    
002100     03 FILLER               PIC X.                                       
002200     03 SALDO-KDTECKEN-BUFF-OF                                            
002300                             PIC X.                                       
002400      88 SALDO-KDTECKEN-PLUS VALUE '+'                                    
002500                             ' '.                                         
002600      88 SALDO-KDTECKEN-MINUS                                             
002700                             VALUE '-'.                                   
002800*                                 PLUS, MINUSTECKEN ALT ASTERISK          
002900     03 FILLER               PIC X(2).                                    
003000     03 SALDO-KVBUFF-OF      PIC X(7).                                    
003100*                                 BUFFERSALDO OFÖRÄDLAT GODS              
003200     03 FILLER               PIC X.                                       
003300     03 SALDO-KDTECKEN-KLI-F PIC X.                                       
003400      88 SALDO-KDTECKEN-PLUS VALUE '+'                                    
003500                             ' '.                                         
003600      88 SALDO-KDTECKEN-MINUS                                             
003700                             VALUE '-'.                                   
003800*                                 PLUS, MINUSTECKEN ALT ASTERISK          
003900     03 FILLER               PIC X.                                       
004000     03 SALDO-KVKOLLI-F      PIC X(4).                                    
004100*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
004200     03 FILLER               PIC X.                                       
004300     03 SALDO-KDTECKEN-KLI-OF                                             
004400                             PIC X.                                       
004500      88 SALDO-KDTECKEN-PLUS VALUE '+'                                    
004600                             ' '.                                         
004700      88 SALDO-KDTECKEN-MINUS                                             
004800                             VALUE '-'.                                   
004900*                                 PLUS, MINUSTECKEN ALT ASTERISK          
005000     03 FILLER               PIC X.                                       
005100     03 SALDO-KVKOLLI-OF     PIC X(4).                                    
005200*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
005300     03 FILLER               PIC X(27).                                   
005400*** END COPY W488010CC0  LENGTH=80                                        
