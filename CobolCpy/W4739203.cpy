000100 01  W4739203.                                                            
000200*                                 LISTFIL, EMBALLAGEPROFORMA              
000300     03 IDDISTR              PIC S9(5)           COMP-3.                  
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000600*                                 VOLVOS KUNDNUMMER  IDKUNDNR-002         
000700*                                 HOS LEVERANTÖR                          
000800     03 IDPRODNR             PIC S9(7)           COMP-3.                  
000900*                                 PRODUKTIONSNUMMER                       
001000     03 KDORDKL              PIC S9              COMP-3.                  
001100*                                 ORDERKLASS                              
001200     03 IDAVD                PIC S9(5)           COMP-3.                  
001300*                                 DEN ANSTÄLLDES AVDELNING/               
001400*                                 KOSTNADSSTÄLLE                          
001500     03 EMBTAB.                                                           
001600*                                 EMBALLAGEÅTGÅNGS-TABELL                 
001700        05 EMBKLASS          OCCURS 4 TIMES.                              
001800           07 KVEMBTYP       OCCURS 24 TIMES                              
001900                             PIC S9(5)           COMP-3.                  
002000*                                 FÖRBRUKNINGMÄNGD AV EMBALLAGE           
002100     03 BEGMT-RAD1           PIC X(35).                                   
002200*                                 GODSMOTTAGARNAMN RAD 1                  
002300     03 BEGMT-RAD2           PIC X(35).                                   
002400*                                 GODSMOTTAGARNAMN RAD 2                  
002500     03 ADGMT-GATA           PIC X(35).                                   
002600*                                 GODSMOTTAGARADRESS GATA                 
002700     03 ADGMT-PADR           PIC X(35).                                   
002800*                                 GODSMOTTAGARADRESS POSTADRESS           
002900     03 ADGMT-LAND           PIC X(35).                                   
003000*                                 GODSMOTTAGARADRESS LAND                 
003100     03 ID-TEXT              PIC X(10).                                   
003200     03 IDEMBNR              PIC S9(7)           COMP-3.                  
003300*                                 EMBALLAGENUMMER                         
003400*** END OF VILMAII-COPY LENGTH= 492 BYTES                                 
