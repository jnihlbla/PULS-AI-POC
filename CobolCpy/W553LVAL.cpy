000100*** EDIT ALLOWED                                                          
000200*                                                                         
000300*    TABELL ATT OMVANDLA LEV.REGS LANDKOD (KDVALLEV) TILL ISOKOD          
000400*    ANVÄNDS I FÖLJANDE PGM: W21326,W21328,W24240,W50111,W602KRUP         
000500*    OBS: EMU-LÄNDERNAS VALUTAKOD SÄTTS TILL EUR.                         
000600*                                                                         
000700 01  VALUTAKOD-CTABELL.                                                   
000800   03  VAL-IX                  PIC S9(9)   VALUE ZERO COMP SYNC.          
000900   03  VAL-IX-MAX              PIC S9(9)   VALUE +35  COMP SYNC.          
001000*                                                                         
001100   03  VAL-INFO.                                                          
001200     05 FILLER                 PIC X(8) VALUE '001SEKSE'.                 
001300     05 FILLER                 PIC X(8) VALUE '002NOKNO'.                 
001400     05 FILLER                 PIC X(8) VALUE '003DKKDK'.                 
001500     05 FILLER                 PIC X(8) VALUE '006EURFI'.                 
001600     05 FILLER                 PIC X(8) VALUE '009EURDE'.                 
001700     05 FILLER                 PIC X(8) VALUE '011PLNPL'.                 
001800     05 FILLER                 PIC X(8) VALUE '014EURNL'.                 
001900     05 FILLER                 PIC X(8) VALUE '015EURBE'.                 
002000     05 FILLER                 PIC X(8) VALUE '017GBPGB'.                 
002100     05 FILLER                 PIC X(8) VALUE '019EURIE'.                 
002200     05 FILLER                 PIC X(8) VALUE '020EURFR'.                 
002300     05 FILLER                 PIC X(8) VALUE '021EURES'.                 
002400     05 FILLER                 PIC X(8) VALUE '022EURPT'.                 
002500     05 FILLER                 PIC X(8) VALUE '024EURIT'.                 
002600     05 FILLER                 PIC X(8) VALUE '025CHFCH'.                 
002700     05 FILLER                 PIC X(8) VALUE '026EURAT'.                 
002800     05 FILLER                 PIC X(8) VALUE '027CZKCZ'.                 
002900     05 FILLER                 PIC X(8) VALUE '028HUFHU'.                 
003000     05 FILLER                 PIC X(8) VALUE '029YUNYU'.                 
003100     05 FILLER                 PIC X(8) VALUE '032TRLTR'.                 
003200     05 FILLER                 PIC X(8) VALUE '033EURGR'.                 
003300     05 FILLER                 PIC X(8) VALUE '034EURSK'.                 
003400     05 FILLER                 PIC X(8) VALUE '044EUREU'.                 
003500     05 FILLER                 PIC X(8) VALUE '101MYRMY'.                 
003600     05 FILLER                 PIC X(8) VALUE '107TWDTW'.                 
003700     05 FILLER                 PIC X(8) VALUE '108HKDHK'.                 
003800     05 FILLER                 PIC X(8) VALUE '109JPYJP'.                 
003900     05 FILLER                 PIC X(8) VALUE '112SGDSG'.                 
004000     05 FILLER                 PIC X(8) VALUE '120CADCA'.                 
004100     05 FILLER                 PIC X(8) VALUE '121USDUS'.                 
004200     05 FILLER                 PIC X(8) VALUE '125MXNMX'.                 
004300     05 FILLER                 PIC X(8) VALUE '154NCZNC'.                 
004400     05 FILLER                 PIC X(8) VALUE '170AUDAU'.                 
004410     05 FILLER                 PIC X(8) VALUE '171CNYCN'.                 
004420     05 FILLER                 PIC X(8) VALUE '172KRWKR'.                 
004500   03  TABELL REDEFINES VAL-INFO OCCURS 35.                               
004600     05  VAL-KDVALUTA          PIC 9(3).                                  
004700     05  VAL-KDVALISO          PIC X(3).                                  
004800     05  VAL-IDLANDX2          PIC X(2).                                  
