000100 01  FKOLLI-W461S012-CTX.                                                 
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 FAKTURA-KOLLI INFO TILL NOAC            
000400     03 FKOLLI-SOR0-IDDISTR  PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 FKOLLI-SOR0-IDKUNDNR PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 FKOLLI-SOR0-IDRONR   PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 FKOLLI-SOR0-TIRODAT  PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 FKOLLI-SOR0-IDPTYP   PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 FKOLLI-SOR0-IDLOPNR  PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 FKOLLI-W461012-CTX.                                               
001700*                                 FAKTURA-KOLLI TILL NOAC PT-012          
001800        05 FKOLLI-IDPTYP     PIC X(3).                                    
001900*                                 POSTTYP                                 
002000        05 FKOLLI-IDDISTR    PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200        05 FKOLLI-IDKUNDNR   PIC S9(7)           COMP-3.                  
002300*                                 KUNDNUMMER                              
002400        05 FKOLLI-KDFAKTYP   PIC X.                                       
002500*                                 FAKTURATYP                              
002600        05 FKOLLI-IDFAKT     PIC S9(7)           COMP-3.                  
002700*                                 FAKTURANUMMER                           
002800        05 FKOLLI-KDSORT2    PIC S9(3)           COMP-3.                  
002900*                                 SORTERINGSFÄLT                          
003000        05 FKOLLI-IDPRODNR   PIC S9(7)           COMP-3.                  
003100*                                 PRODUKTIONSNUMMER                       
003200        05 FKOLLI-IDKOLLI    PIC S9(5)           COMP-3.                  
003300*                                 KOLLINUMMER                             
003400        05 FKOLLI-IDARTNR    PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 FKOLLI-IDORDNR    PIC S9(7)           COMP-3.                  
003700*                                 ORDERNR             IDORDNR-002         
003800        05 FKOLLI-VKORDBTO-KOLLI                                          
003900                             PIC S9(6)V9(1)      COMP-3.                  
004000*                                 ORDERVIKT BRUTTO PER KOLLI              
004100        05 FKOLLI-VLORDBTO-KOLLI                                          
004200                             PIC S9(4)V9(3)      COMP-3.                  
004300*                                 ORDERVOLYM BRUTTO KOLLI                 
004400        05 FKOLLI-IDLBBET    PIC X(12).                                   
004500*                                 LASTBÄRARBETECKNING                     
004600        05 FKOLLI-KDEMBTYP   PIC S9(3)           COMP-3.                  
004700*                                 EMBALLAGETYP       KDEMBTYP-002         
004800        05 FKOLLI-IDTRPBO.                                                
004900*                                 BOLLA-DOKUMENT IDENTITET                
005000           07 FKOLLI-IDTRPBOT                                             
005100                             PIC X.                                       
005200*                                 BOLLA-DOKUMENT TECKEN                   
005300           07 FKOLLI-IDTRPBON                                             
005400                             PIC S9(7)           COMP-3.                  
005500*                                 BOLLA-DOKUMENT NUMMER                   
005600        05 FKOLLI-IDFAKT-GNB PIC X(8).                                    
005700*                                 FAKTURANUMMER GNB                       
005800*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
