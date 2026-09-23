000100 01  W371SORT.                                                            
000200*                                 GENERELL  INLEDNINGSPOST FÖR            
000300*                                 PROGRAM W37140                          
000400*                                                                         
000500*                                 FÖR IDLISTA GÄLLER:                     
000600*                                   001 = EXTERN LISTA SVERIGE            
000700*                                   002 = INTERN LISTA SVERIGE            
000800*                                   003 = INTERN LISTA ENG. C2            
000900*                                   005 = EXTERN LISTA ENG.               
001000*                                   006 = INTERN LISTA SVENSKA NO         
001100*                                   007 = EXTERN LISTA SVERIGE NO         
001200*                                   008 = EJ INKOMNA RETURER/ANSK         
001300*                                   010 = FEL OCH VARNINGSLISTA           
001400*                                                                         
001500*                                 FÖR IDPTYP  GÄLLER:                     
001600*                                   ENDAST FÖR IDLISTA 001 - 007          
001700*                                   001 = ADRESSPOST                      
001800*                                   002 = EJ INKOMNA RETURER              
001900*                                   003 = REG. ORDER UTAN OBJEKT          
002000*                                   004 = REG. OBJEKT DENNA VECKA         
002100*                                   005 = UTF. KVITTN.DENNA VECKA         
002200*                                   006 = BORTTAGNA OBJ UNDER PER         
002300*                                   008 = EJ INKOMNA RETURER/ANSK         
002400*                                                                         
002500*                                 S O R T E R I N G S N Y C K E L         
002600*                                  S1 = LISTNR     S2 = POSTTYP           
002700*                                  S3 = KONCERN    S4 = DISTRIKT          
002800*                                  S5 = KUND       S6 = ANSK. NR          
002900*                                  S7 = PRODSL     S8 = SE TEXT           
003000*                                  S9 = SE TEXT                           
003100*                                                                         
003200*                                  S  S S S S S S      S        S         
003300*                                  1  2 3 4 5 6 7      8        9         
003400*                                                                         
003500*                                  1  1 X X 0 0 0   ZERO     ZERO         
003600*                                     2 X X X 0 0  ARTNR  REG DAT         
003700*                                     3 X X X 0 0 OBJEKT  REG DAT         
003800*                                     4 X X X 0 0 OBJEKT  REG DAT         
003900*                                     5 X X X 0 0 OBJEKT  REG DAT         
004000*                                     6 X X X 0 0 OBJEKT  REG DAT         
004100*                                  2  1 0 X 0 0 X   ZERO     ZERO         
004200*                                     2 0 X X 0 X  ARTNR  REG DAT         
004300*                                     3 0 X X 0 X OBJEKT  REG DAT         
004400*                                     4 0 X X 0 X OBJEKT  REG DAT         
004500*                                     5 0 X X 0 X OBJEKT  REG DAT         
004600*                                     6 0 X X 0 X OBJEKT  REG DAT         
004700*                                  3  1 0 X 0 0 X   ZERO     ZERO         
004800*                                     2 0 X X 0 X  ARTNR  REG DAT         
004900*                                     3 0 X X 0 X OBJEKT  REG DAT         
005000*                                     4 0 X X 0 X OBJEKT  REG DAT         
005100*                                     5 0 X X 0 X OBJEKT  REG DAT         
005200*                                     6 0 X X 0 X OBJEKT  REG DAT         
005300*                                  5  1 X X 0 0 0   ZERO     ZERO         
005400*                                     2 X X X 0 0  ARTNR  REG DAT         
005500*                                     3 X X X 0 0 OBJEKT  REG DAT         
005600*                                     4 X X X 0 0 OBJEKT  REG DAT         
005700*                                     5 X X X 0 0 OBJEKT  REG DAT         
005800*                                     6 X X X 0 0 OBJEKT  REG DAT         
005900*                                  6  1 0 X 0 0 X   ZERO     ZERO         
006000*                                     2 0 X X 0 X  ARTNR  REG DAT         
006100*                                     3 0 X X 0 X OBJEKT  REG DAT         
006200*                                     4 0 X X 0 X OBJEKT  REG DAT         
006300*                                     5 0 X X 0 X OBJEKT  REG DAT         
006400*                                     6 0 X X 0 X OBJEKT  REG DAT         
006500*                                  7  1 X X 0 0 0   ZERO     ZERO         
006600*                                     2 X X X 0 0  ARTNR  REG DAT         
006700*                                     3 X X X 0 0 OBJEKT  REG DAT         
006800*                                     4 X X X 0 0 OBJEKT  REG DAT         
006900*                                     5 X X X 0 0 OBJEKT  REG DAT         
007000*                                     6 X X X 0 0 OBJEKT  REG DAT         
007100*                                  8  0 0 0 0 X X  ARTNR DISTRIKT         
007200*                                 10  0 0 X X 0 0 ORDERNR ART/OBJ         
007300*                                                                         
007400     03 IDLISTA              PIC S9(3)           COMP-3.                  
007500*                                 LISTNUMMER                              
007600     03 IDKONCNR             PIC S9(3)           COMP-3.                  
007700*                                 KONCERNNUMMER                           
007800     03 IDDISTR              PIC S9(5)           COMP-3.                  
007900*                                 DISTRIKTNUMMER                          
008000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
008100*                                 KUNDNUMMER                              
008200     03 KUNDREF              PIC X(10).                                   
008300*                                 KUNDENS REFERENS (ORDERID)              
008400     03 IDANSK               PIC S9(3)           COMP-3.                  
008500*                                 ANSKAFFARNUMMER                         
008600     03 KDPRODSL             PIC S9(3)           COMP-3.                  
008700*                                 PRODUKTSLAG                             
008800     03 IDPTYP               PIC X(3).                                    
008900*                                 POSTTYP                                 
009000     03 SORTARG1             PIC X(10).                                   
009100     03 SORTARG2             PIC X(10).                                   
009200*                                                                         
009300     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
009400*                                 OBJEKTNUMMER                            
009500     03 REKSIFFR             PIC S9              COMP-3.                  
009600*                                 KONTROLLSIFFRA                          
009700     03 BEART-OBJ            PIC X(25).                                   
009800*                                 ARTIKELBENÄMNING                        
009900     03 IDDC                 PIC X(2).                                    
010000*                                 IDENTIFIERARE LAGER                     
010100     03 IDORDNR              PIC S9(5)           COMP-3.                  
010200*                                 ORDERNUMMER                             
010300     03 KVRETUR-REST         PIC S9(5)           COMP-3.                  
010400*                                 RETURNERAD EJ AVBOKAD                   
010500*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
