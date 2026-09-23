000100 01  FKOLLI-W461012-CTX.                                                  
000200*                                 FAKTURA-KOLLI TILL NOAC PT-012          
000300     03 FKOLLI-IDPTYP        PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 FKOLLI-IDDISTR       PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 FKOLLI-IDKUNDNR      PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 FKOLLI-KDFAKTYP      PIC X.                                       
001000*                                 FAKTURATYP                              
001100     03 FKOLLI-IDFAKT        PIC S9(7)           COMP-3.                  
001200*                                 FAKTURANUMMER                           
001300     03 FKOLLI-KDSORT2       PIC S9(3)           COMP-3.                  
001400*                                 SORTERINGSFÄLT                          
001500     03 FKOLLI-IDPRODNR      PIC S9(7)           COMP-3.                  
001600*                                 PRODUKTIONSNUMMER                       
001700     03 FKOLLI-IDKOLLI       PIC S9(5)           COMP-3.                  
001800*                                 KOLLINUMMER                             
001900     03 FKOLLI-IDARTNR       PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100     03 FKOLLI-IDORDNR       PIC S9(7)           COMP-3.                  
002200*                                 ORDERNR             IDORDNR-002         
002300     03 FKOLLI-VKORDBTO-KOLLI                                             
002400                             PIC S9(6)V9(1)      COMP-3.                  
002500*                                 ORDERVIKT BRUTTO PER KOLLI              
002600     03 FKOLLI-VLORDBTO-KOLLI                                             
002700                             PIC S9(4)V9(3)      COMP-3.                  
002800*                                 ORDERVOLYM BRUTTO KOLLI                 
002900     03 FKOLLI-IDLBBET       PIC X(12).                                   
003000*                                 LASTBÄRARBETECKNING                     
003100     03 FKOLLI-KDEMBTYP      PIC S9(3)           COMP-3.                  
003200*                                 EMBALLAGETYP       KDEMBTYP-002         
003300     03 FKOLLI-IDTRPBO.                                                   
003400*                                 BOLLA-DOKUMENT IDENTITET                
003500        05 FKOLLI-IDTRPBOT   PIC X.                                       
003600*                                 BOLLA-DOKUMENT TECKEN                   
003700        05 FKOLLI-IDTRPBON   PIC S9(7)           COMP-3.                  
003800*                                 BOLLA-DOKUMENT NUMMER                   
003900     03 FKOLLI-IDFAKT-GNB    PIC X(8).                                    
004000*                                 FAKTURANUMMER GNB                       
004100*** END OF VILMAII-COPY LENGTH= 68 BYTES                                  
