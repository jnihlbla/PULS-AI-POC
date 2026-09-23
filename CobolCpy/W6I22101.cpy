000100 01  MID-W6I22101.                                                        
000200*                                 COPYTEXT FÖR MID W6I22101               
000300*                                                                         
000400     03 MID-IDLEVNR-IN       PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600     03 MID-IDLEVNR-UT       PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 MID-IDLEVG-IN        PIC X(5).                                    
000900*                                 LEVERANTÖRS GODSADRESS NUMMER           
001000     03 MID-IDLEVG-UT        PIC X(5).                                    
001100*                                 LEVERANTÖRS GODSADRESS NUMMER           
001200     03 MID-IDLEVG-ENTER     PIC X(5).                                    
001300*                                 LEVERANTÖRS GODSADRESS NUMMER           
001400     03 MID-IDLEVG-NEXT      PIC X(5).                                    
001500*                                 LEVERANTÖRS GODSADRESS NUMMER           
001600     03 MID-IDLEVG-SPAR      PIC X(5).                                    
001700*                                 LEVERANTÖRS GODSADRESS NUMMER           
001800     03 MID-INPUT.                                                        
001900*                                                                         
002000        05 MID-FLSKPLOT      PIC X.                                       
002100*                                 SKIPLOT KONTROLL ?                      
002200        05 MID-KDCMD-LEV     PIC X.                                       
002300         88 MID-KDCMD-INGENTING                                           
002400                             VALUE ' '.                                   
002500         88 MID-KDCMD-DELETE VALUE 'D'                                    
002600                             'B'.                                         
002700         88 MID-KDCMD-REPLACE                                             
002800                             VALUE 'R'                                    
002900                             'Ä'.                                         
003000         88 MID-KDCMD-INSERT VALUE 'I'                                    
003100                             'N'.                                         
003200         88 MID-KDCMD-SELECT VALUE 'S'                                    
003300                             'V'.                                         
003400*                                 RAD-UPPDATERINGSKOMMANDO                
003500*                                  BLANK  = INGENTING                     
003600*                                  D , B  = DELETE                        
003700*                                  R , Ä  = REPLACE                       
003800*                                  I , N  = INSERT                        
003900*                                  S , V  = SELECT                        
004000        05 MID-IDMAIL1       PIC X(60).                                   
004100*                                 MAIL ADRESS                             
004200        05 MID-IDMAIL2       PIC X(60).                                   
004300*                                 MAIL ADRESS                             
004400        05 MID-IDMAIL3       PIC X(60).                                   
004500*                                 MAIL ADRESS                             
004600        05 MID-BELEV         PIC X(35).                                   
004700*                                 LEVERANTÖRSNAMN                         
004800        05 MID-KDCMD-GADR    PIC X.                                       
004900         88 MID-KDCMD-INGENTING                                           
005000                             VALUE ' '.                                   
005100         88 MID-KDCMD-DELETE VALUE 'D'                                    
005200                             'B'.                                         
005300         88 MID-KDCMD-REPLACE                                             
005400                             VALUE 'R'                                    
005500                             'Ä'.                                         
005600         88 MID-KDCMD-INSERT VALUE 'I'                                    
005700                             'N'.                                         
005800         88 MID-KDCMD-SELECT VALUE 'S'                                    
005900                             'V'.                                         
006000*                                 RAD-UPPDATERINGSKOMMANDO                
006100*                                  BLANK  = INGENTING                     
006200*                                  D , B  = DELETE                        
006300*                                  R , Ä  = REPLACE                       
006400*                                  I , N  = INSERT                        
006500*                                  S , V  = SELECT                        
006600        05 MID-ADLEV-RAD1    PIC X(35).                                   
006700*                                 LEVERANTÖRSADRESS                       
006800        05 MID-ADLEV-RAD2    PIC X(35).                                   
006900*                                 LEVERANTÖRSADRESS                       
007000        05 MID-IDLEVFAX-1    PIC X(16).                                   
007100*                                 TELEFAXNUMMER TILL LEVERANTÖR           
007200        05 MID-ADLEV-ORT     PIC X(35).                                   
007300*                                 LEVERANTÖRSADRESS ORT                   
007400        05 MID-IDLEVFAX-2    PIC X(16).                                   
007500*                                 TELEFAXNUMMER TILL LEVERANTÖR           
007600        05 MID-ADLEVLND      PIC X(20).                                   
007700*                                 LEVERANTÖRSADRESS LAND                  
007800        05 MID-IDLEVFAX-3    PIC X(16).                                   
007900*                                 TELEFAXNUMMER TILL LEVERANTÖR           
008000        05 MID-IDLEVTLF      PIC X(20).                                   
008100*                                 TELEFONNUMMER TILL LEVERANTÖR           
008200        05 MID-IDLEVFAX-4    PIC X(16).                                   
008300*                                 TELEFAXNUMMER TILL LEVERANTÖR           
008400        05 MID-ADATTENT-Q    PIC X(40).                                   
008500*                                 ATTENTIONADRESS                         
008600        05 MID-ADATTENT-A    PIC X(40).                                   
008700*                                 ATTENTIONADRESS                         
008800*** END OF VILMAII-COPY LENGTH= 542 BYTES                                 
