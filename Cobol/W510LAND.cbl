000100*COMPOPT STDSUB=YES                                                       
000200                                                                          
000300 ID DIVISION.                                                             
000400                                                                          
000500 PROGRAM-ID.    W510LAND.                                                 
000600                                                                          
000700*    AUTHOR         KARL JOHAN HANSSON.                                   
000800*    DATE-WRITTEN.  AUG 2002.                                             
000900*                                                                         
001000*    REMARKS                                                              
001100*                                                                         
001200*    FUNKTION: ANROP   - "CALL W510LAND USING LAND-W510LAND"              
001300*                                                                         
001400*        BEHANDLING    - DENNA MODUL OMVANDLAR TVÅSTÄLLIG LAND-           
001500*                        KOD TILL 35-STÄLLIG LANDBETECKNING PÅ            
001600*                        SVENSKA FÖR UPPDATERING AV BETALAR-              
001700*                        REGISTER M. FL. LIKNANDE UPPDRAG.                
001800*                                                                         
001900*              ANROP:  - FYLL I IDLANDX2 MED TVÅSTÄLLIG LAND-             
002000*                        KOD (ISO) T.EX. 'SE'                             
002100*                                                                         
002200*              SVAR:   - VID TRÄFF I TABELLEN FÅS SOM SVAR                
002300*                        LANDETS BETECKNING I LAND-BELAND-SVE             
002400*                        PÅ SVENSKA.                                      
002500*                      - ANNARS ÄR LAND-BELAND-SVE BLANKT.                
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                PIC X(8)    VALUE 'W510LAND'.                   
003300 77  JA                   PIC X       VALUE 'J'.                          
003400 77  NEJ                  PIC X       VALUE 'N'.                          
003500                                                                          
003600 01  LAND-RADER.                                                          
003700   03 FILLER PIC X(37) VALUE 'AEFÖRENADE ARABMIRATEN         '.           
003800   03 FILLER PIC X(37) VALUE 'ALALBANIEN                     '.           
003900   03 FILLER PIC X(37) VALUE 'ANVÄSTINDIEN, NEDERLÄNDSKA     '.           
004000   03 FILLER PIC X(37) VALUE 'AOANGOLA                       '.           
004100   03 FILLER PIC X(37) VALUE 'ARARGENTINA                    '.           
004200   03 FILLER PIC X(37) VALUE 'ATÖSTERRIKE                    '.           
004300   03 FILLER PIC X(37) VALUE 'AUAUSTRALIEN                   '.           
004400   03 FILLER PIC X(37) VALUE 'AZAZERBAIJAN                   '.           
004500   03 FILLER PIC X(37) VALUE 'BDBANGLADESH                   '.           
004600   03 FILLER PIC X(37) VALUE 'BEBELGIEN                      '.           
004700   03 FILLER PIC X(37) VALUE 'BGBULGARIEN                    '.           
004800   03 FILLER PIC X(37) VALUE 'BHBAHRAIN                      '.           
004900   03 FILLER PIC X(37) VALUE 'BNBRUNEI                       '.           
005000   03 FILLER PIC X(37) VALUE 'BOBOLIVIA                      '.           
005100   03 FILLER PIC X(37) VALUE 'BRBRASILIEN                    '.           
005200   03 FILLER PIC X(37) VALUE 'BYVITRYSSLAND                  '.           
005300   03 FILLER PIC X(37) VALUE 'CAKANADA                       '.           
005400   03 FILLER PIC X(37) VALUE 'CHSCHWEIZ                      '.           
005500   03 FILLER PIC X(37) VALUE 'CIELFENBENSKUSTEN              '.           
005600   03 FILLER PIC X(37) VALUE 'CLCHILE                        '.           
005700   03 FILLER PIC X(37) VALUE 'CNKINA                         '.           
005800   03 FILLER PIC X(37) VALUE 'COCOLOMBIA                     '.           
005900   03 FILLER PIC X(37) VALUE 'CRCOSTA RICA                   '.           
006000   03 FILLER PIC X(37) VALUE 'CSTJECKOSLOVAKIEN              '.           
006100   03 FILLER PIC X(37) VALUE 'CUKUBA                         '.           
006200   03 FILLER PIC X(37) VALUE 'CYCYPERN                       '.           
006300   03 FILLER PIC X(37) VALUE 'CZTJECKIEN                     '.           
006400   03 FILLER PIC X(37) VALUE 'DDTYSKA DEMOKRATISKA REPUBLIKEN'.           
006500   03 FILLER PIC X(37) VALUE 'DETYSKLAND, FÖRBUNDSREPUBLIK   '.           
006600   03 FILLER PIC X(37) VALUE 'DKDANMARK                      '.           
006700   03 FILLER PIC X(37) VALUE 'DODOMINIKANSKA REPUBLIKEN      '.           
006800   03 FILLER PIC X(37) VALUE 'DZALGERIET                     '.           
006900   03 FILLER PIC X(37) VALUE 'ECECUADOR                      '.           
007000   03 FILLER PIC X(37) VALUE 'EEESTLAND                      '.           
007100   03 FILLER PIC X(37) VALUE 'EGEGYPTEN                      '.           
007200   03 FILLER PIC X(37) VALUE 'ESSPANIEN                      '.           
007300   03 FILLER PIC X(37) VALUE 'ETETIOPIEN                     '.           
007400   03 FILLER PIC X(37) VALUE 'FIFINLAND                      '.           
007500   03 FILLER PIC X(37) VALUE 'FJFIJI                         '.           
007600   03 FILLER PIC X(37) VALUE 'FRFRANKRIKE                    '.           
007700   03 FILLER PIC X(37) VALUE 'GBSTORBRITANNIEN OCH NORDIRLAND'.           
007800   03 FILLER PIC X(37) VALUE 'GEGEORGIEN                     '.           
007900   03 FILLER PIC X(37) VALUE 'GFGUYANA, FRANSKA DEPARTEMENTET'.           
008000   03 FILLER PIC X(37) VALUE 'GHGHANA                        '.           
008100   03 FILLER PIC X(37) VALUE 'GPGUADELOUPE                   '.           
008200   03 FILLER PIC X(37) VALUE 'GRGREKLAND                     '.           
008300   03 FILLER PIC X(37) VALUE 'GTGUATEMALA                    '.           
008400   03 FILLER PIC X(37) VALUE 'HKHONGKONG                     '.           
008500   03 FILLER PIC X(37) VALUE 'HNHONDURAS                     '.           
008600   03 FILLER PIC X(37) VALUE 'HTHAITI                        '.           
008700   03 FILLER PIC X(37) VALUE 'HUUNGERN                       '.           
008800   03 FILLER PIC X(37) VALUE 'IDINDONESIEN                   '.           
008900   03 FILLER PIC X(37) VALUE 'IEIRLAND                       '.           
009000   03 FILLER PIC X(37) VALUE 'ILISRAEL                       '.           
009100   03 FILLER PIC X(37) VALUE 'ININDIEN                       '.           
009200   03 FILLER PIC X(37) VALUE 'IRIRAN                         '.           
009300   03 FILLER PIC X(37) VALUE 'ISISLAND                       '.           
009400   03 FILLER PIC X(37) VALUE 'ITITALIEN                      '.           
009500   03 FILLER PIC X(37) VALUE 'JMJAMAICA                      '.           
009600   03 FILLER PIC X(37) VALUE 'JOJORDANIEN                    '.           
009700   03 FILLER PIC X(37) VALUE 'JPJAPAN                        '.           
009800   03 FILLER PIC X(37) VALUE 'KEKENYA                        '.           
009900   03 FILLER PIC X(37) VALUE                                              
010000                        'KP KOREA,DEMOKRATISKA FOLKREPUBLIKEN'.           
010100   03 FILLER PIC X(37) VALUE 'KRKOREA, REPUBLIKEN            '.           
010200   03 FILLER PIC X(37) VALUE 'KWKUWAIT                       '.           
010300   03 FILLER PIC X(37) VALUE 'KZKAZAKHSTAN                   '.           
010400   03 FILLER PIC X(37) VALUE 'LBLIBANON                      '.           
010500   03 FILLER PIC X(37) VALUE 'LKSRI LANKA                    '.           
010600   03 FILLER PIC X(37) VALUE 'LRLIBERIA                      '.           
010700   03 FILLER PIC X(37) VALUE 'LTLITAUEN                      '.           
010800   03 FILLER PIC X(37) VALUE 'LULUXENBURG                    '.           
010900   03 FILLER PIC X(37) VALUE 'LVLETTLAND                     '.           
010910   03 FILLER PIC X(37) VALUE 'LYLIBYEN                       '.           
011000   03 FILLER PIC X(37) VALUE 'MAMAROCKO                      '.           
011010   03 FILLER PIC X(37) VALUE 'MDMOLDAVIEN                    '.           
011100   03 FILLER PIC X(37) VALUE 'MQMARTINIQUE                   '.           
011200   03 FILLER PIC X(37) VALUE 'MTMALTA                        '.           
011210   03 FILLER PIC X(37) VALUE 'MUMAURITIUS                    '.           
011300   03 FILLER PIC X(37) VALUE 'MWMALAWI                       '.           
011400   03 FILLER PIC X(37) VALUE 'MXMEXIKO                       '.           
011500   03 FILLER PIC X(37) VALUE 'MYMALAYSIA                     '.           
011600   03 FILLER PIC X(37) VALUE 'MZMOZAMBIQUE                   '.           
011700   03 FILLER PIC X(37) VALUE 'NANAMIBIA                      '.           
011800   03 FILLER PIC X(37) VALUE 'NCNYA KALEDONIEN               '.           
011900   03 FILLER PIC X(37) VALUE 'NGNIGERIA                      '.           
012000   03 FILLER PIC X(37) VALUE 'NINICARAGUA                    '.           
012100   03 FILLER PIC X(37) VALUE 'NLNEDERLÄNDERNA                '.           
012200   03 FILLER PIC X(37) VALUE 'NONORGE                        '.           
012300   03 FILLER PIC X(37) VALUE 'NZNYA ZEELAND                  '.           
012400   03 FILLER PIC X(37) VALUE 'OMOMAN                         '.           
012500   03 FILLER PIC X(37) VALUE 'PAPANAMA                       '.           
012600   03 FILLER PIC X(37) VALUE 'PEPERU                         '.           
012700   03 FILLER PIC X(37) VALUE 'PFPOLYNESIEN,FRANSKA           '.           
012800   03 FILLER PIC X(37) VALUE 'PHFILIPPINERNA                 '.           
012900   03 FILLER PIC X(37) VALUE 'PKPAKISTAN                     '.           
013000   03 FILLER PIC X(37) VALUE 'PLPOLEN                        '.           
013100   03 FILLER PIC X(37) VALUE 'PRPUERTO RICO                  '.           
013200   03 FILLER PIC X(37) VALUE 'PTPORTUGAL                     '.           
013300   03 FILLER PIC X(37) VALUE 'PYPARAGUAY                     '.           
013400   03 FILLER PIC X(37) VALUE 'QAQATAR                        '.           
013500   03 FILLER PIC X(37) VALUE 'REREUNION                      '.           
013600   03 FILLER PIC X(37) VALUE 'RORUMÄNIEN                     '.           
013700   03 FILLER PIC X(37) VALUE 'RURYSSLAND                     '.           
013800   03 FILLER PIC X(37) VALUE 'RWRWANDA                       '.           
013900   03 FILLER PIC X(37) VALUE 'SASAUDI-ARABIEN                '.           
014000   03 FILLER PIC X(37) VALUE 'SESVERIGE                      '.           
014100   03 FILLER PIC X(37) VALUE 'SGSINGAPORE                    '.           
014200   03 FILLER PIC X(37) VALUE 'SISLOVENIEN                    '.           
014300   03 FILLER PIC X(37) VALUE 'SKSLOVAKIEN                    '.           
014400   03 FILLER PIC X(37) VALUE 'SUSOVJETUNIONEN                '.           
014500   03 FILLER PIC X(37) VALUE 'SVEL SALVADOR                  '.           
014600   03 FILLER PIC X(37) VALUE 'SYSYRIEN                       '.           
014700   03 FILLER PIC X(37) VALUE 'THTHAILAND                     '.           
014710   03 FILLER PIC X(37) VALUE 'TMTURKMENISTAN                 '.           
014800   03 FILLER PIC X(37) VALUE 'TNTUNISIEN                     '.           
014900   03 FILLER PIC X(37) VALUE 'TRTURKIET                      '.           
015000   03 FILLER PIC X(37) VALUE 'TTTRINIDAD OCH TOBAGO          '.           
015100   03 FILLER PIC X(37) VALUE 'TWTAIWAN                       '.           
015200   03 FILLER PIC X(37) VALUE 'TZTANZANIA                     '.           
015300   03 FILLER PIC X(37) VALUE 'UAUKRAINA                      '.           
015400   03 FILLER PIC X(37) VALUE 'UGUGANDA                       '.           
015500   03 FILLER PIC X(37) VALUE 'USUSA                          '.           
015700   03 FILLER PIC X(37) VALUE 'UYURUGUAY                      '.           
015800   03 FILLER PIC X(37) VALUE 'VEVENEZUELA                    '.           
015900   03 FILLER PIC X(37) VALUE 'VGJUNGFRUÖARNA, BRITTISKA      '.           
016000   03 FILLER PIC X(37) VALUE 'VNVIETNAM                      '.           
016100   03 FILLER PIC X(37) VALUE 'YEYEMEN, ARABREPUBLIK          '.           
016300   03 FILLER PIC X(37) VALUE 'YUJUGOSLAVIEN                  '.           
016400   03 FILLER PIC X(37) VALUE 'ZASYDAFRIKA                    '.           
016500   03 FILLER PIC X(37) VALUE 'ZMZAMBIA                       '.           
016600   03 FILLER PIC X(37) VALUE 'ZWZIMBABWE                     '.           
016700 01  FILLER REDEFINES LAND-RADER.                                         
016800   03  LAND-TABELL     OCCURS 131 INDEXED BY IX.                          
016900     05  TAB-IDLANDX2       PIC X(2).                                     
017000     05  TAB-BELAND-SVE     PIC X(35).                                    
017100     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300                                                                          
017400*01  -COPY W510LAND                                                       
017500     EJECT                                                                
017600 PROCEDURE DIVISION USING LAND-W510LAND.                                  
017700                                                                          
017800     SET IX TO 1                                                          
017900     SEARCH LAND-TABELL                                                   
018000        AT END                                                            
018100           MOVE  SPACE                          TO LAND-BELAND-SVE        
018200        WHEN LAND-IDLANDX2 = TAB-IDLANDX2(IX)                             
018300           MOVE TAB-BELAND-SVE(IX)              TO LAND-BELAND-SVE        
018400     END-SEARCH                                                           
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
