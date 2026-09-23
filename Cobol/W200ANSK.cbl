000010*COMPOPT STDSUB=YES                                                       
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W200ANSK.                                                
000700 AUTHOR.        JAN PETTERSSON.                                           
000800 DATE-WRITTEN.  MAJ 1978.                                                 
000810 DATE-COMPILED.                                                           
000900*    REMARKS.                                                             
001100*        ANROP                                                   *        
001200*                        SKER GENOM CALL W200ANSK                *        
001300*        BEHANDLING                                              *        
001400*                        DENNA MODUL OMVANDLAR ANSKAFFARNR       *        
001500*                        TILL TABELL-INDEX RESP GER TEXT-        *        
001600*                        STRÄNG TILL LISTOR                      *        
001610*        OBS !!          MÅNDAGSANALYSEN ANVÄNDER INTE DETTA     *        
001620*                        PROGRAM, ÄNDRAS SEPARAT                 *        
001700*        LÄNKAREA                                                *        
001800*                        W009W42                                 *        
002000     EJECT                                                                
002100******************************************************************        
002200*                                                                *        
002300*       GRUPP    SEKTION      FUNKTION        OMRÅDE             *        
002400*       00-49    0-99         0-99            0-99               *        
002500*       50-99    0-99         0-99            0-99               *        
002600*      100-299   RES PV       RES PV          PV                 *        
002800*      300-399   RES PV       RES PV          PV                 *        
003500*      400-599   RES PV       RES PV          PV                 *        
003600*      600-699   RES PV       RES PV          PV                 *        
003700*      700-799   RES PV       RES PV          PV                 *        
003800*      800-899   RES PV       RES PV          PV                 *        
003900*      900-909   LAND-ROVER   SV BIL          PV                 *        
004000*      910-919   RENAULT      SV BIL          PV                 *        
004100*      920-929   TILLB PV     TILLB PV        PV                 *        
004200*      930-939   TILLB PV     TILLB PV        PV                 *        
004300*      940-949   TILLB PV     TILLB PV        PV                 *        
004400*      950-959   TILLB PV     TILLB PV        PV                 *        
004500*      960-969   FÖRP         FÖRP            FÖRP               *        
004600*      970-999   VAKANT       VAKANT          VAKANT             *        
004700*                                                                *        
004800******************************************************************        
004900     EJECT                                                                
005000 ENVIRONMENT DIVISION.                                                    
005100 DATA DIVISION.                                                           
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005301                                                                          
005310*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                   PIC X(16)   VALUE 'W0094200'.                
005500 01  TABELL-ANSK-INDEX.                                                   
005600     03  TAB-STORLEK-INDEX.                                               
005700         05  FILLER       PIC 9(18)   VALUE    000001001001001001.        
005800         05  FILLER       PIC 9(18)   VALUE    001001001001001001.        
005900         05  FILLER       PIC 9(18)   VALUE    002001001001001001.        
006000         05  FILLER       PIC 9(18)   VALUE    003001001001001001.        
006100         05  FILLER       PIC 9(18)   VALUE    004001001001001001.        
006200         05  FILLER       PIC 9(18)   VALUE    005001001001001001.        
006300         05  FILLER       PIC 9(18)   VALUE    006001001001001001.        
006400         05  FILLER       PIC 9(18)   VALUE    007001001001001001.        
006500         05  FILLER       PIC 9(18)   VALUE    008001001001001001.        
006600         05  FILLER       PIC 9(18)   VALUE    009001001001001001.        
006610*        05  ******      OVANFÖR FINNS ANSKAFFARE 09  *****               
006700         05  FILLER       PIC 9(18)   VALUE    010001001001001001.        
006900         05  FILLER       PIC 9(18)   VALUE    011001001001001001.        
007000         05  FILLER       PIC 9(18)   VALUE    012001001001001001.        
007100         05  FILLER       PIC 9(18)   VALUE    013001001001001001.        
007200         05  FILLER       PIC 9(18)   VALUE    014001001001001001.        
007300         05  FILLER       PIC 9(18)   VALUE    015001001001001001.        
007400         05  FILLER       PIC 9(18)   VALUE    016001001001001001.        
007500         05  FILLER       PIC 9(18)   VALUE    017001001001001001.        
007600         05  FILLER       PIC 9(18)   VALUE    018001001001001001.        
007700         05  FILLER       PIC 9(18)   VALUE    019001001001001001.        
007710*        05  ******      OVANFÖR FINNS ANSKAFFARE 19                      
007800         05  FILLER       PIC 9(18)   VALUE    020001001001001001.        
008000         05  FILLER       PIC 9(18)   VALUE    021001001001001001.        
008100         05  FILLER       PIC 9(18)   VALUE    022001001001001001.        
008200         05  FILLER       PIC 9(18)   VALUE    023001001001001001.        
008300         05  FILLER       PIC 9(18)   VALUE    024001001001001001.        
008400         05  FILLER       PIC 9(18)   VALUE    025001001001001001.        
008500         05  FILLER       PIC 9(18)   VALUE    026001001001001001.        
008600         05  FILLER       PIC 9(18)   VALUE    027001001001001001.        
008700         05  FILLER       PIC 9(18)   VALUE    028001001001001001.        
008800         05  FILLER       PIC 9(18)   VALUE    029001001001001001.        
008810*        05  ******      OVANFÖR FINNS ANSKAFFARE 29  *****               
008900         05  FILLER       PIC 9(18)   VALUE    030001001001001001.        
009100         05  FILLER       PIC 9(18)   VALUE    031001001001001001.        
009200         05  FILLER       PIC 9(18)   VALUE    032001001001001001.        
009300         05  FILLER       PIC 9(18)   VALUE    033001001001001001.        
009400         05  FILLER       PIC 9(18)   VALUE    034001001001001001.        
009500         05  FILLER       PIC 9(18)   VALUE    035001001001001001.        
009600         05  FILLER       PIC 9(18)   VALUE    036001001001001001.        
009700         05  FILLER       PIC 9(18)   VALUE    037001001001001001.        
009800         05  FILLER       PIC 9(18)   VALUE    038001001001001001.        
009900         05  FILLER       PIC 9(18)   VALUE    039001001001001001.        
009910*        05  ******      OVANFÖR FINNS ANSKAFFARE 39  *****               
010000         05  FILLER       PIC 9(18)   VALUE    040001001001001001.        
010200         05  FILLER       PIC 9(18)   VALUE    041001001001001001.        
010300         05  FILLER       PIC 9(18)   VALUE    042001001001001001.        
010400         05  FILLER       PIC 9(18)   VALUE    043001001001001001.        
010500         05  FILLER       PIC 9(18)   VALUE    044001001001001001.        
010600         05  FILLER       PIC 9(18)   VALUE    045001001001001001.        
010700         05  FILLER       PIC 9(18)   VALUE    046001001001001001.        
010800         05  FILLER       PIC 9(18)   VALUE    047001001001001001.        
010900         05  FILLER       PIC 9(18)   VALUE    048001001001001001.        
011000         05  FILLER       PIC 9(18)   VALUE    049001001001001001.        
011010*        05  ******      OVANFÖR FINNS ANSKAFFARE 49  *****               
011100         05  FILLER       PIC 9(18)   VALUE    050002001001001001.        
011300         05  FILLER       PIC 9(18)   VALUE    051002001001001001.        
011400         05  FILLER       PIC 9(18)   VALUE    052002001001001001.        
011500         05  FILLER       PIC 9(18)   VALUE    053002001001001001.        
011600         05  FILLER       PIC 9(18)   VALUE    054002001001001001.        
011700         05  FILLER       PIC 9(18)   VALUE    055002001001001001.        
011800         05  FILLER       PIC 9(18)   VALUE    056002001001001001.        
011900         05  FILLER       PIC 9(18)   VALUE    057002001001001001.        
012000         05  FILLER       PIC 9(18)   VALUE    058002001001001001.        
012100         05  FILLER       PIC 9(18)   VALUE    059002001001001001.        
012110*        05  ******      OVANFÖR FINNS ANSKAFFARE 59  *****               
012200         05  FILLER       PIC 9(18)   VALUE    060002001001001001.        
012400         05  FILLER       PIC 9(18)   VALUE    061002001001001001.        
012500         05  FILLER       PIC 9(18)   VALUE    062002001001001001.        
012600         05  FILLER       PIC 9(18)   VALUE    063002001001001001.        
012700         05  FILLER       PIC 9(18)   VALUE    064002001001001001.        
012800         05  FILLER       PIC 9(18)   VALUE    065002001001001001.        
012900         05  FILLER       PIC 9(18)   VALUE    066002001001001001.        
013000         05  FILLER       PIC 9(18)   VALUE    067002001001001001.        
013100         05  FILLER       PIC 9(18)   VALUE    068002001001001001.        
013200         05  FILLER       PIC 9(18)   VALUE    069002001001001001.        
013210*        05  ******      OVANFÖR FINNS ANSKAFFARE 69  *****               
013300         05  FILLER       PIC 9(18)   VALUE    070002001001001001.        
013500         05  FILLER       PIC 9(18)   VALUE    071002001001001001.        
013600         05  FILLER       PIC 9(18)   VALUE    072002001001001001.        
013700         05  FILLER       PIC 9(18)   VALUE    073002001001001001.        
013800         05  FILLER       PIC 9(18)   VALUE    074002001001001001.        
013900         05  FILLER       PIC 9(18)   VALUE    075002001001001001.        
014000         05  FILLER       PIC 9(18)   VALUE    076002001001001001.        
014100         05  FILLER       PIC 9(18)   VALUE    077002001001001001.        
014200         05  FILLER       PIC 9(18)   VALUE    078002001001001001.        
014300         05  FILLER       PIC 9(18)   VALUE    079002001001001001.        
014310*        05  ******      OVANFÖR FINNS ANSKAFFARE 79  *****               
014400         05  FILLER       PIC 9(18)   VALUE    080002001001001001.        
014600         05  FILLER       PIC 9(18)   VALUE    081002001001001001.        
014700         05  FILLER       PIC 9(18)   VALUE    082002001001001001.        
014800         05  FILLER       PIC 9(18)   VALUE    083002001001001001.        
014900         05  FILLER       PIC 9(18)   VALUE    084002001001001001.        
015000         05  FILLER       PIC 9(18)   VALUE    085002001001001001.        
015100         05  FILLER       PIC 9(18)   VALUE    086002001001001001.        
015200         05  FILLER       PIC 9(18)   VALUE    087002001001001001.        
015300         05  FILLER       PIC 9(18)   VALUE    088002001001001001.        
015400         05  FILLER       PIC 9(18)   VALUE    089002001001001001.        
015410*        05  ******      OVANFÖR FINNS ANSKAFFARE 89  *****               
015500         05  FILLER       PIC 9(18)   VALUE    090002001001001001.        
015700         05  FILLER       PIC 9(18)   VALUE    091002001001001001.        
015800         05  FILLER       PIC 9(18)   VALUE    092002001001001001.        
015900         05  FILLER       PIC 9(18)   VALUE    093002001001001001.        
016000         05  FILLER       PIC 9(18)   VALUE    094002001001001001.        
016100         05  FILLER       PIC 9(18)   VALUE    095002001001001001.        
016200         05  FILLER       PIC 9(18)   VALUE    096002001001001001.        
016300         05  FILLER       PIC 9(18)   VALUE    097002001001001001.        
016400         05  FILLER       PIC 9(18)   VALUE    098002001001001001.        
016500         05  FILLER       PIC 9(18)   VALUE    099002001001001001.        
016510*        05  ******      OVANFÖR FINNS ANSKAFFARE 99  *****               
016600         05  FILLER       PIC 9(18)   VALUE    100003002002002001.        
016800         05  FILLER       PIC 9(18)   VALUE    101003002002002001.        
016900         05  FILLER       PIC 9(18)   VALUE    102003002002002001.        
017000         05  FILLER       PIC 9(18)   VALUE    103003002002002001.        
017100         05  FILLER       PIC 9(18)   VALUE    104003002002002001.        
017200         05  FILLER       PIC 9(18)   VALUE    105003002002002001.        
017300         05  FILLER       PIC 9(18)   VALUE    106003002002002001.        
017400         05  FILLER       PIC 9(18)   VALUE    107003002002002001.        
017500         05  FILLER       PIC 9(18)   VALUE    108003002002002001.        
017600         05  FILLER       PIC 9(18)   VALUE    109003002002002001.        
017610*        05  ******      OVANFÖR FINNS ANSKAFFARE 109 *****               
017700         05  FILLER       PIC 9(18)   VALUE    110003002002002001.        
017900         05  FILLER       PIC 9(18)   VALUE    111003002002002001.        
018000         05  FILLER       PIC 9(18)   VALUE    112003002002002001.        
018100         05  FILLER       PIC 9(18)   VALUE    113003002002002001.        
018200         05  FILLER       PIC 9(18)   VALUE    114003002002002001.        
018300         05  FILLER       PIC 9(18)   VALUE    115003002002002001.        
018400         05  FILLER       PIC 9(18)   VALUE    116003002002002001.        
018500         05  FILLER       PIC 9(18)   VALUE    117003002002002001.        
018600         05  FILLER       PIC 9(18)   VALUE    118003002002002001.        
018700         05  FILLER       PIC 9(18)   VALUE    119003002002002001.        
018710*        05  ******      OVANFÖR FINNS ANSKAFFARE 119 *****               
018800         05  FILLER       PIC 9(18)   VALUE    120003002002002001.        
019000         05  FILLER       PIC 9(18)   VALUE    121003002002002001.        
019100         05  FILLER       PIC 9(18)   VALUE    122003002002002001.        
019200         05  FILLER       PIC 9(18)   VALUE    123003002002002001.        
019300         05  FILLER       PIC 9(18)   VALUE    124003002002002001.        
019400         05  FILLER       PIC 9(18)   VALUE    125003002002002001.        
019500         05  FILLER       PIC 9(18)   VALUE    126003002002002001.        
019600         05  FILLER       PIC 9(18)   VALUE    127003002002002001.        
019700         05  FILLER       PIC 9(18)   VALUE    128003002002002001.        
019800         05  FILLER       PIC 9(18)   VALUE    129003002002002001.        
019810*        05  ******      OVANFÖR FINNS ANSKAFFARE 129 *****               
019900         05  FILLER       PIC 9(18)   VALUE    130003002002002001.        
020100         05  FILLER       PIC 9(18)   VALUE    131003002002002001.        
020200         05  FILLER       PIC 9(18)   VALUE    132003002002002001.        
020300         05  FILLER       PIC 9(18)   VALUE    133003002002002001.        
020400         05  FILLER       PIC 9(18)   VALUE    134003002002002001.        
020500         05  FILLER       PIC 9(18)   VALUE    135003002002002001.        
020600         05  FILLER       PIC 9(18)   VALUE    136003002002002001.        
020700         05  FILLER       PIC 9(18)   VALUE    137003002002002001.        
020800         05  FILLER       PIC 9(18)   VALUE    138003002002002001.        
020900         05  FILLER       PIC 9(18)   VALUE    139003002002002001.        
020910*        05  ******      OVANFÖR FINNS ANSKAFFARE 139 *****               
021000         05  FILLER       PIC 9(18)   VALUE    140003002002002001.        
021200         05  FILLER       PIC 9(18)   VALUE    141003002002002001.        
021300         05  FILLER       PIC 9(18)   VALUE    142003002002002001.        
021400         05  FILLER       PIC 9(18)   VALUE    143003002002002001.        
021500         05  FILLER       PIC 9(18)   VALUE    144003002002002001.        
021600         05  FILLER       PIC 9(18)   VALUE    145003002002002001.        
021700         05  FILLER       PIC 9(18)   VALUE    146003002002002001.        
021800         05  FILLER       PIC 9(18)   VALUE    147003002002002001.        
021900         05  FILLER       PIC 9(18)   VALUE    148003002002002001.        
022000         05  FILLER       PIC 9(18)   VALUE    149003002002002001.        
022010*        05  ******      OVANFÖR FINNS ANSKAFFARE 149 *****               
022100         05  FILLER       PIC 9(18)   VALUE    150003002002002001.        
022300         05  FILLER       PIC 9(18)   VALUE    151003002002002001.        
022400         05  FILLER       PIC 9(18)   VALUE    152003002002002001.        
022500         05  FILLER       PIC 9(18)   VALUE    153003002002002001.        
022600         05  FILLER       PIC 9(18)   VALUE    154003002002002001.        
022700         05  FILLER       PIC 9(18)   VALUE    155003002002002001.        
022800         05  FILLER       PIC 9(18)   VALUE    156003002002002001.        
022900         05  FILLER       PIC 9(18)   VALUE    157003002002002001.        
023000         05  FILLER       PIC 9(18)   VALUE    158003002002002001.        
023100         05  FILLER       PIC 9(18)   VALUE    159003002002002001.        
023110*        05  ******      OVANFÖR FINNS ANSKAFFARE 159 *****               
023200         05  FILLER       PIC 9(18)   VALUE    160003002002002001.        
023400         05  FILLER       PIC 9(18)   VALUE    161003002002002001.        
023500         05  FILLER       PIC 9(18)   VALUE    162003002002002001.        
023600         05  FILLER       PIC 9(18)   VALUE    163003002002002001.        
023700         05  FILLER       PIC 9(18)   VALUE    164003002002002001.        
023800         05  FILLER       PIC 9(18)   VALUE    165003002002002001.        
023900         05  FILLER       PIC 9(18)   VALUE    166003002002002001.        
024000         05  FILLER       PIC 9(18)   VALUE    167003002002002001.        
024100         05  FILLER       PIC 9(18)   VALUE    168003002002002001.        
024200         05  FILLER       PIC 9(18)   VALUE    169003002002002001.        
024210*        05  ******      OVANFÖR FINNS ANSKAFFARE 169 *****               
024300         05  FILLER       PIC 9(18)   VALUE    170003002002002001.        
024500         05  FILLER       PIC 9(18)   VALUE    171003002002002001.        
024600         05  FILLER       PIC 9(18)   VALUE    172003002002002001.        
024700         05  FILLER       PIC 9(18)   VALUE    173003002002002001.        
024800         05  FILLER       PIC 9(18)   VALUE    174003002002002001.        
024900         05  FILLER       PIC 9(18)   VALUE    175003002002002001.        
025000         05  FILLER       PIC 9(18)   VALUE    176003002002002001.        
025100         05  FILLER       PIC 9(18)   VALUE    177003002002002001.        
025200         05  FILLER       PIC 9(18)   VALUE    178003002002002001.        
025300         05  FILLER       PIC 9(18)   VALUE    179003002002002001.        
025310*        05  ******      OVANFÖR FINNS ANSKAFFARE 179 *****               
025400         05  FILLER       PIC 9(18)   VALUE    180003002002002001.        
025600         05  FILLER       PIC 9(18)   VALUE    181003002002002001.        
025700         05  FILLER       PIC 9(18)   VALUE    182003002002002001.        
025800         05  FILLER       PIC 9(18)   VALUE    183003002002002001.        
025900         05  FILLER       PIC 9(18)   VALUE    184003002002002001.        
026000         05  FILLER       PIC 9(18)   VALUE    185003002002002001.        
026100         05  FILLER       PIC 9(18)   VALUE    186003002002002001.        
026200         05  FILLER       PIC 9(18)   VALUE    187003002002002001.        
026300         05  FILLER       PIC 9(18)   VALUE    188003002002002001.        
026400         05  FILLER       PIC 9(18)   VALUE    189003002002002001.        
026410*        05  ******      OVANFÖR FINNS ANSKAFFARE 189 *****               
026500         05  FILLER       PIC 9(18)   VALUE    190003002002002001.        
026700         05  FILLER       PIC 9(18)   VALUE    191003002002002001.        
026800         05  FILLER       PIC 9(18)   VALUE    192003002002002001.        
026900         05  FILLER       PIC 9(18)   VALUE    193003002002002001.        
027000         05  FILLER       PIC 9(18)   VALUE    194003002002002001.        
027100         05  FILLER       PIC 9(18)   VALUE    195003002002002001.        
027200         05  FILLER       PIC 9(18)   VALUE    196003002002002001.        
027300         05  FILLER       PIC 9(18)   VALUE    197003002002002001.        
027400         05  FILLER       PIC 9(18)   VALUE    198003002002002001.        
027500         05  FILLER       PIC 9(18)   VALUE    199003002002002001.        
027510*        05  ******      OVANFÖR FINNS ANSKAFFARE 199 *****               
027600         05  FILLER       PIC 9(18)   VALUE    200003002002002001.        
027800         05  FILLER       PIC 9(18)   VALUE    201003002002002001.        
027900         05  FILLER       PIC 9(18)   VALUE    202003002002002001.        
028000         05  FILLER       PIC 9(18)   VALUE    203003002002002001.        
028100         05  FILLER       PIC 9(18)   VALUE    204003002002002001.        
028200         05  FILLER       PIC 9(18)   VALUE    205003002002002001.        
028300         05  FILLER       PIC 9(18)   VALUE    206003002002002001.        
028400         05  FILLER       PIC 9(18)   VALUE    207003002002002001.        
028500         05  FILLER       PIC 9(18)   VALUE    208003002002002001.        
028600         05  FILLER       PIC 9(18)   VALUE    209003002002002001.        
028610*        05  ******      OVANFÖR FINNS ANSKAFFARE 209 *****               
028700         05  FILLER       PIC 9(18)   VALUE    210003002002002001.        
028900         05  FILLER       PIC 9(18)   VALUE    211003002002002001.        
029000         05  FILLER       PIC 9(18)   VALUE    212003002002002001.        
029100         05  FILLER       PIC 9(18)   VALUE    213003002002002001.        
029200         05  FILLER       PIC 9(18)   VALUE    214003002002002001.        
029300         05  FILLER       PIC 9(18)   VALUE    215003002002002001.        
029400         05  FILLER       PIC 9(18)   VALUE    216003002002002001.        
029500         05  FILLER       PIC 9(18)   VALUE    217003002002002001.        
029600         05  FILLER       PIC 9(18)   VALUE    218003002002002001.        
029700         05  FILLER       PIC 9(18)   VALUE    219003002002002001.        
029710*        05  ******      OVANFÖR FINNS ANSKAFFARE 219 *****               
029800         05  FILLER       PIC 9(18)   VALUE    220003002002002001.        
030000         05  FILLER       PIC 9(18)   VALUE    221003002002002001.        
030100         05  FILLER       PIC 9(18)   VALUE    222003002002002001.        
030200         05  FILLER       PIC 9(18)   VALUE    223003002002002001.        
030300         05  FILLER       PIC 9(18)   VALUE    224003002002002001.        
030400         05  FILLER       PIC 9(18)   VALUE    225003002002002001.        
030500         05  FILLER       PIC 9(18)   VALUE    226003002002002001.        
030600         05  FILLER       PIC 9(18)   VALUE    227003002002002001.        
030700         05  FILLER       PIC 9(18)   VALUE    228003002002002001.        
030800         05  FILLER       PIC 9(18)   VALUE    229003002002002001.        
030810*        05  ******      OVANFÖR FINNS ANSKAFFARE 229 *****               
030900         05  FILLER       PIC 9(18)   VALUE    230003002002002001.        
031100         05  FILLER       PIC 9(18)   VALUE    231003002002002001.        
031200         05  FILLER       PIC 9(18)   VALUE    232003002002002001.        
031300         05  FILLER       PIC 9(18)   VALUE    233003002002002001.        
031400         05  FILLER       PIC 9(18)   VALUE    234003002002002001.        
031500         05  FILLER       PIC 9(18)   VALUE    235003002002002001.        
031600         05  FILLER       PIC 9(18)   VALUE    236003002002002001.        
031700         05  FILLER       PIC 9(18)   VALUE    237003002002002001.        
031800         05  FILLER       PIC 9(18)   VALUE    238003002002002001.        
031900         05  FILLER       PIC 9(18)   VALUE    239003002002002001.        
031910*        05  ******      OVANFÖR FINNS ANSKAFFARE 239 *****               
032000         05  FILLER       PIC 9(18)   VALUE    240003002002002001.        
032200         05  FILLER       PIC 9(18)   VALUE    241003002002002001.        
032300         05  FILLER       PIC 9(18)   VALUE    242003002002002001.        
032400         05  FILLER       PIC 9(18)   VALUE    243003002002002001.        
032500         05  FILLER       PIC 9(18)   VALUE    244003002002002001.        
032600         05  FILLER       PIC 9(18)   VALUE    245003002002002001.        
032700         05  FILLER       PIC 9(18)   VALUE    246003002002002001.        
032800         05  FILLER       PIC 9(18)   VALUE    247003002002002001.        
032900         05  FILLER       PIC 9(18)   VALUE    248003002002002001.        
033000         05  FILLER       PIC 9(18)   VALUE    249003002002002001.        
033010*        05  ******      OVANFÖR FINNS ANSKAFFARE 249 *****               
033100         05  FILLER       PIC 9(18)   VALUE    250003002002002001.        
033300         05  FILLER       PIC 9(18)   VALUE    251003002002002001.        
033400         05  FILLER       PIC 9(18)   VALUE    252003002002002001.        
033500         05  FILLER       PIC 9(18)   VALUE    253003002002002001.        
033600         05  FILLER       PIC 9(18)   VALUE    254003002002002001.        
033700         05  FILLER       PIC 9(18)   VALUE    255003002002002001.        
033800         05  FILLER       PIC 9(18)   VALUE    256003002002002001.        
033900         05  FILLER       PIC 9(18)   VALUE    257003002002002001.        
034000         05  FILLER       PIC 9(18)   VALUE    258003002002002001.        
034100         05  FILLER       PIC 9(18)   VALUE    259003002002002001.        
034110*        05  ******      OVANFÖR FINNS ANSKAFFARE 259 *****               
034200         05  FILLER       PIC 9(18)   VALUE    260003002002002001.        
034400         05  FILLER       PIC 9(18)   VALUE    261003002002002001.        
034500         05  FILLER       PIC 9(18)   VALUE    262003002002002001.        
034600         05  FILLER       PIC 9(18)   VALUE    263003002002002001.        
034700         05  FILLER       PIC 9(18)   VALUE    264003002002002001.        
034800         05  FILLER       PIC 9(18)   VALUE    265003002002002001.        
034900         05  FILLER       PIC 9(18)   VALUE    266003002002002001.        
035000         05  FILLER       PIC 9(18)   VALUE    267003002002002001.        
035100         05  FILLER       PIC 9(18)   VALUE    268003002002002001.        
035200         05  FILLER       PIC 9(18)   VALUE    269003002002002001.        
035210*        05  ******      OVANFÖR FINNS ANSKAFFARE 269 *****               
035300         05  FILLER       PIC 9(18)   VALUE    270003002002002001.        
035500         05  FILLER       PIC 9(18)   VALUE    271003002002002001.        
035600         05  FILLER       PIC 9(18)   VALUE    272003002002002001.        
035700         05  FILLER       PIC 9(18)   VALUE    273003002002002001.        
035800         05  FILLER       PIC 9(18)   VALUE    274003002002002001.        
035900         05  FILLER       PIC 9(18)   VALUE    275003002002002001.        
036000         05  FILLER       PIC 9(18)   VALUE    276003002002002001.        
036100         05  FILLER       PIC 9(18)   VALUE    277003002002002001.        
036200         05  FILLER       PIC 9(18)   VALUE    278003002002002001.        
036300         05  FILLER       PIC 9(18)   VALUE    279003002002002001.        
036310*        05  ******      OVANFÖR FINNS ANSKAFFARE 279 *****               
036400         05  FILLER       PIC 9(18)   VALUE    280003002002002001.        
036600         05  FILLER       PIC 9(18)   VALUE    281003002002002001.        
036700         05  FILLER       PIC 9(18)   VALUE    282003002002002001.        
036800         05  FILLER       PIC 9(18)   VALUE    283003002002002001.        
036900         05  FILLER       PIC 9(18)   VALUE    284003002002002001.        
037000         05  FILLER       PIC 9(18)   VALUE    285003002002002001.        
037100         05  FILLER       PIC 9(18)   VALUE    286003002002002001.        
037200         05  FILLER       PIC 9(18)   VALUE    287003002002002001.        
037300         05  FILLER       PIC 9(18)   VALUE    288003002002002001.        
037400         05  FILLER       PIC 9(18)   VALUE    289003002002002001.        
037410*        05  ******      OVANFÖR FINNS ANSKAFFARE 289 *****               
037500         05  FILLER       PIC 9(18)   VALUE    290003002002002001.        
037700         05  FILLER       PIC 9(18)   VALUE    291003002002002001.        
037800         05  FILLER       PIC 9(18)   VALUE    292003002002002001.        
037900         05  FILLER       PIC 9(18)   VALUE    293003002002002001.        
038000         05  FILLER       PIC 9(18)   VALUE    294003002002002001.        
038100         05  FILLER       PIC 9(18)   VALUE    295003002002002001.        
038200         05  FILLER       PIC 9(18)   VALUE    296003002002002001.        
038300         05  FILLER       PIC 9(18)   VALUE    297003002002002001.        
038400         05  FILLER       PIC 9(18)   VALUE    298003002002002001.        
038500         05  FILLER       PIC 9(18)   VALUE    299003002002002001.        
038510*        05  ******      OVANFÖR FINNS ANSKAFFARE 299 *****               
038600         05  FILLER       PIC 9(18)   VALUE    300004002002002001.        
038800         05  FILLER       PIC 9(18)   VALUE    301004002002002001.        
038900         05  FILLER       PIC 9(18)   VALUE    302004002002002001.        
039000         05  FILLER       PIC 9(18)   VALUE    303004002002002001.        
039100         05  FILLER       PIC 9(18)   VALUE    304004002002002001.        
039200         05  FILLER       PIC 9(18)   VALUE    305004002002002001.        
039300         05  FILLER       PIC 9(18)   VALUE    306004002002002001.        
039400         05  FILLER       PIC 9(18)   VALUE    307004002002002001.        
039500         05  FILLER       PIC 9(18)   VALUE    308004002002002001.        
039600         05  FILLER       PIC 9(18)   VALUE    309004002002002001.        
039610*        05  ******      OVANFÖR FINNS ANSKAFFARE 309 *****               
039700         05  FILLER       PIC 9(18)   VALUE    310004002002002001.        
039900         05  FILLER       PIC 9(18)   VALUE    311004002002002001.        
040000         05  FILLER       PIC 9(18)   VALUE    312004002002002001.        
040100         05  FILLER       PIC 9(18)   VALUE    313004002002002001.        
040200         05  FILLER       PIC 9(18)   VALUE    314004002002002001.        
040300         05  FILLER       PIC 9(18)   VALUE    315004002002002001.        
040400         05  FILLER       PIC 9(18)   VALUE    316004002002002001.        
040500         05  FILLER       PIC 9(18)   VALUE    317004002002002001.        
040600         05  FILLER       PIC 9(18)   VALUE    318004002002002001.        
040700         05  FILLER       PIC 9(18)   VALUE    319004002002002001.        
040710*        05  ******      OVANFÖR FINNS ANSKAFFARE 319 *****               
040800         05  FILLER       PIC 9(18)   VALUE    320004002002002001.        
041000         05  FILLER       PIC 9(18)   VALUE    321004002002002001.        
041100         05  FILLER       PIC 9(18)   VALUE    322004002002002001.        
041200         05  FILLER       PIC 9(18)   VALUE    323004002002002001.        
041300         05  FILLER       PIC 9(18)   VALUE    324004002002002001.        
041400         05  FILLER       PIC 9(18)   VALUE    325004002002002001.        
041500         05  FILLER       PIC 9(18)   VALUE    326004002002002001.        
041600         05  FILLER       PIC 9(18)   VALUE    327004002002002001.        
041700         05  FILLER       PIC 9(18)   VALUE    328004002002002001.        
041800         05  FILLER       PIC 9(18)   VALUE    329004002002002001.        
041810*        05  ******      OVANFÖR FINNS ANSKAFFARE 329 *****               
041900         05  FILLER       PIC 9(18)   VALUE    330004002002002001.        
042100         05  FILLER       PIC 9(18)   VALUE    331004002002002001.        
042200         05  FILLER       PIC 9(18)   VALUE    332004002002002001.        
042300         05  FILLER       PIC 9(18)   VALUE    333004002002002001.        
042400         05  FILLER       PIC 9(18)   VALUE    334004002002002001.        
042500         05  FILLER       PIC 9(18)   VALUE    335004002002002001.        
042600         05  FILLER       PIC 9(18)   VALUE    336004002002002001.        
042700         05  FILLER       PIC 9(18)   VALUE    337004002002002001.        
042800         05  FILLER       PIC 9(18)   VALUE    338004002002002001.        
042900         05  FILLER       PIC 9(18)   VALUE    339004002002002001.        
042910*        05  ******      OVANFÖR FINNS ANSKAFFARE 339 *****               
043000         05  FILLER       PIC 9(18)   VALUE    340004002002002001.        
043200         05  FILLER       PIC 9(18)   VALUE    341004002002002001.        
043300         05  FILLER       PIC 9(18)   VALUE    342004002002002001.        
043400         05  FILLER       PIC 9(18)   VALUE    343004002002002001.        
043500         05  FILLER       PIC 9(18)   VALUE    344004002002002001.        
043600         05  FILLER       PIC 9(18)   VALUE    345004002002002001.        
043700         05  FILLER       PIC 9(18)   VALUE    346004002002002001.        
043800         05  FILLER       PIC 9(18)   VALUE    347004002002002001.        
043900         05  FILLER       PIC 9(18)   VALUE    348004002002002001.        
044000         05  FILLER       PIC 9(18)   VALUE    349004002002002001.        
044010*        05  ******      OVANFÖR FINNS ANSKAFFARE 349 *****               
044100         05  FILLER       PIC 9(18)   VALUE    350004002002002001.        
044300         05  FILLER       PIC 9(18)   VALUE    351004002002002001.        
044400         05  FILLER       PIC 9(18)   VALUE    352004002002002001.        
044500         05  FILLER       PIC 9(18)   VALUE    353004002002002001.        
044600         05  FILLER       PIC 9(18)   VALUE    354004002002002001.        
044700         05  FILLER       PIC 9(18)   VALUE    355004002002002001.        
044800         05  FILLER       PIC 9(18)   VALUE    356004002002002001.        
044900         05  FILLER       PIC 9(18)   VALUE    357004002002002001.        
045000         05  FILLER       PIC 9(18)   VALUE    358004002002002001.        
045100         05  FILLER       PIC 9(18)   VALUE    359004002002002001.        
045110*        05  ******      OVANFÖR FINNS ANSKAFFARE 359 *****               
045200         05  FILLER       PIC 9(18)   VALUE    360004002002002001.        
045400         05  FILLER       PIC 9(18)   VALUE    361004002002002001.        
045500         05  FILLER       PIC 9(18)   VALUE    362004002002002001.        
045600         05  FILLER       PIC 9(18)   VALUE    363004002002002001.        
045700         05  FILLER       PIC 9(18)   VALUE    364004002002002001.        
045800         05  FILLER       PIC 9(18)   VALUE    365004002002002001.        
045900         05  FILLER       PIC 9(18)   VALUE    366004002002002001.        
046000         05  FILLER       PIC 9(18)   VALUE    367004002002002001.        
046100         05  FILLER       PIC 9(18)   VALUE    368004002002002001.        
046200         05  FILLER       PIC 9(18)   VALUE    369004002002002001.        
046210*        05  ******      OVANFÖR FINNS ANSKAFFARE 369 *****               
046300         05  FILLER       PIC 9(18)   VALUE    370004002002002001.        
046500         05  FILLER       PIC 9(18)   VALUE    371004002002002001.        
046600         05  FILLER       PIC 9(18)   VALUE    372004002002002001.        
046700         05  FILLER       PIC 9(18)   VALUE    373004002002002001.        
046800         05  FILLER       PIC 9(18)   VALUE    374004002002002001.        
046900         05  FILLER       PIC 9(18)   VALUE    375004002002002001.        
047000         05  FILLER       PIC 9(18)   VALUE    376004002002002001.        
047100         05  FILLER       PIC 9(18)   VALUE    377004002002002001.        
047200         05  FILLER       PIC 9(18)   VALUE    378004002002002001.        
047300         05  FILLER       PIC 9(18)   VALUE    379004002002002001.        
047310*        05  ******      OVANFÖR FINNS ANSKAFFARE 379 *****               
047400         05  FILLER       PIC 9(18)   VALUE    380004002002002001.        
047600         05  FILLER       PIC 9(18)   VALUE    381004002002002001.        
047700         05  FILLER       PIC 9(18)   VALUE    382004002002002001.        
047800         05  FILLER       PIC 9(18)   VALUE    383004002002002001.        
047900         05  FILLER       PIC 9(18)   VALUE    384004002002002001.        
048000         05  FILLER       PIC 9(18)   VALUE    385004002002002001.        
048100         05  FILLER       PIC 9(18)   VALUE    386004002002002001.        
048200         05  FILLER       PIC 9(18)   VALUE    387004002002002001.        
048300         05  FILLER       PIC 9(18)   VALUE    388004002002002001.        
048400         05  FILLER       PIC 9(18)   VALUE    389004002002002001.        
048410*        05  ******      OVANFÖR FINNS ANSKAFFARE 389 *****               
048500         05  FILLER       PIC 9(18)   VALUE    390004002002002001.        
048700         05  FILLER       PIC 9(18)   VALUE    391004002002002001.        
048800         05  FILLER       PIC 9(18)   VALUE    392004002002002001.        
048900         05  FILLER       PIC 9(18)   VALUE    393004002002002001.        
049000         05  FILLER       PIC 9(18)   VALUE    394004002002002001.        
049100         05  FILLER       PIC 9(18)   VALUE    395004002002002001.        
049200         05  FILLER       PIC 9(18)   VALUE    396004002002002001.        
049300         05  FILLER       PIC 9(18)   VALUE    397004002002002001.        
049400         05  FILLER       PIC 9(18)   VALUE    398004002002002001.        
049500         05  FILLER       PIC 9(18)   VALUE    399004002002002001.        
049510*        05  ******      OVANFÖR FINNS ANSKAFFARE 399 *****               
049600         05  FILLER       PIC 9(18)   VALUE    400005002002002001.        
049800         05  FILLER       PIC 9(18)   VALUE    401005002002002001.        
049900         05  FILLER       PIC 9(18)   VALUE    402005002003002001.        
050000         05  FILLER       PIC 9(18)   VALUE    403005002002002001.        
050100         05  FILLER       PIC 9(18)   VALUE    404005002002002001.        
050200         05  FILLER       PIC 9(18)   VALUE    405005002002002001.        
050300         05  FILLER       PIC 9(18)   VALUE    406005002002002001.        
050400         05  FILLER       PIC 9(18)   VALUE    407005002002002001.        
050500         05  FILLER       PIC 9(18)   VALUE    408005002002002001.        
050600         05  FILLER       PIC 9(18)   VALUE    409005002002002001.        
050610*        05  ******      OVANFÖR FINNS ANSKAFFARE 409 *****               
050700         05  FILLER       PIC 9(18)   VALUE    410005002002002001.        
050900         05  FILLER       PIC 9(18)   VALUE    411005002002002001.        
051000         05  FILLER       PIC 9(18)   VALUE    412005002002002001.        
051100         05  FILLER       PIC 9(18)   VALUE    413005002002002001.        
051200         05  FILLER       PIC 9(18)   VALUE    414005002002002001.        
051300         05  FILLER       PIC 9(18)   VALUE    415005002002002001.        
051400         05  FILLER       PIC 9(18)   VALUE    416005002002002001.        
051500         05  FILLER       PIC 9(18)   VALUE    417005002002002001.        
051600         05  FILLER       PIC 9(18)   VALUE    418005002002002001.        
051700         05  FILLER       PIC 9(18)   VALUE    419005002002002001.        
051710*        05  ******      OVANFÖR FINNS ANSKAFFARE 419 *****               
051800         05  FILLER       PIC 9(18)   VALUE    420005002002002001.        
052000         05  FILLER       PIC 9(18)   VALUE    421005002002002001.        
052100         05  FILLER       PIC 9(18)   VALUE    422005002002002001.        
052200         05  FILLER       PIC 9(18)   VALUE    423005002002002001.        
052300         05  FILLER       PIC 9(18)   VALUE    424005002002002001.        
052400         05  FILLER       PIC 9(18)   VALUE    425005002002002001.        
052500         05  FILLER       PIC 9(18)   VALUE    426005002002002001.        
052600         05  FILLER       PIC 9(18)   VALUE    427005002002002001.        
052700         05  FILLER       PIC 9(18)   VALUE    428005002002002001.        
052800         05  FILLER       PIC 9(18)   VALUE    429005002002002001.        
052810*        05  ******      OVANFÖR FINNS ANSKAFFARE 429 *****               
052900         05  FILLER       PIC 9(18)   VALUE    430005002002002001.        
053100         05  FILLER       PIC 9(18)   VALUE    431005002002002001.        
053200         05  FILLER       PIC 9(18)   VALUE    432005002002002001.        
053300         05  FILLER       PIC 9(18)   VALUE    433005002002002001.        
053400         05  FILLER       PIC 9(18)   VALUE    434005002002002001.        
053500         05  FILLER       PIC 9(18)   VALUE    435005002002002001.        
053600         05  FILLER       PIC 9(18)   VALUE    436005002002002001.        
053700         05  FILLER       PIC 9(18)   VALUE    437005002002002001.        
053800         05  FILLER       PIC 9(18)   VALUE    438005002002002001.        
053900         05  FILLER       PIC 9(18)   VALUE    439005002002002001.        
053910*        05  ******      OVANFÖR FINNS ANSKAFFARE 439 *****               
054000         05  FILLER       PIC 9(18)   VALUE    440005002002002001.        
054200         05  FILLER       PIC 9(18)   VALUE    441005002002002001.        
054300         05  FILLER       PIC 9(18)   VALUE    442005002002002001.        
054400         05  FILLER       PIC 9(18)   VALUE    443005002002002001.        
054500         05  FILLER       PIC 9(18)   VALUE    444005002002002001.        
054600         05  FILLER       PIC 9(18)   VALUE    445005002002002001.        
054700         05  FILLER       PIC 9(18)   VALUE    446005002002002001.        
054800         05  FILLER       PIC 9(18)   VALUE    447005002002002001.        
054900         05  FILLER       PIC 9(18)   VALUE    448005002002002001.        
055000         05  FILLER       PIC 9(18)   VALUE    449005002002002001.        
055010*        05  ******      OVANFÖR FINNS ANSKAFFARE 449 *****               
055100         05  FILLER       PIC 9(18)   VALUE    450005002002002001.        
055300         05  FILLER       PIC 9(18)   VALUE    451005002002002001.        
055400         05  FILLER       PIC 9(18)   VALUE    452005002002002001.        
055500         05  FILLER       PIC 9(18)   VALUE    453005002002002001.        
055600         05  FILLER       PIC 9(18)   VALUE    454005002002002001.        
055700         05  FILLER       PIC 9(18)   VALUE    455005002002002001.        
055800         05  FILLER       PIC 9(18)   VALUE    456005002002002001.        
055900         05  FILLER       PIC 9(18)   VALUE    457005002002002001.        
056000         05  FILLER       PIC 9(18)   VALUE    458005002002002001.        
056100         05  FILLER       PIC 9(18)   VALUE    459005002002002001.        
056110*        05  ******      OVANFÖR FINNS ANSKAFFARE 459 *****               
056200         05  FILLER       PIC 9(18)   VALUE    460005002002002001.        
056400         05  FILLER       PIC 9(18)   VALUE    461005002002002001.        
056500         05  FILLER       PIC 9(18)   VALUE    462005002002002001.        
056600         05  FILLER       PIC 9(18)   VALUE    463005002002002001.        
056700         05  FILLER       PIC 9(18)   VALUE    464005002002002001.        
056800         05  FILLER       PIC 9(18)   VALUE    465005002002002001.        
056900         05  FILLER       PIC 9(18)   VALUE    466005002002002001.        
057000         05  FILLER       PIC 9(18)   VALUE    467005002002002001.        
057100         05  FILLER       PIC 9(18)   VALUE    468005002002002001.        
057200         05  FILLER       PIC 9(18)   VALUE    469005002002002001.        
057210*        05  ******      OVANFÖR FINNS ANSKAFFARE 469 *****               
057300         05  FILLER       PIC 9(18)   VALUE    470005002002002001.        
057500         05  FILLER       PIC 9(18)   VALUE    471005002002002001.        
057600         05  FILLER       PIC 9(18)   VALUE    472005002002002001.        
057700         05  FILLER       PIC 9(18)   VALUE    473005002002002001.        
057800         05  FILLER       PIC 9(18)   VALUE    474005002002002001.        
057900         05  FILLER       PIC 9(18)   VALUE    475005002002002001.        
058000         05  FILLER       PIC 9(18)   VALUE    476005002002002001.        
058100         05  FILLER       PIC 9(18)   VALUE    477005002002002001.        
058200         05  FILLER       PIC 9(18)   VALUE    478005002002002001.        
058300         05  FILLER       PIC 9(18)   VALUE    479005002002002001.        
058310*        05  ******      OVANFÖR FINNS ANSKAFFARE 479 *****               
058400         05  FILLER       PIC 9(18)   VALUE    480005002002002001.        
058600         05  FILLER       PIC 9(18)   VALUE    481005002002002001.        
058700         05  FILLER       PIC 9(18)   VALUE    482005002002002001.        
058800         05  FILLER       PIC 9(18)   VALUE    483005002002002001.        
058900         05  FILLER       PIC 9(18)   VALUE    484005002002002001.        
059000         05  FILLER       PIC 9(18)   VALUE    485005002002002001.        
059100         05  FILLER       PIC 9(18)   VALUE    486005002002002001.        
059200         05  FILLER       PIC 9(18)   VALUE    487005002002002001.        
059300         05  FILLER       PIC 9(18)   VALUE    488005002002002001.        
059400         05  FILLER       PIC 9(18)   VALUE    489005002002002001.        
059410*        05  ******      OVANFÖR FINNS ANSKAFFARE 489 *****               
059500         05  FILLER       PIC 9(18)   VALUE    490005002002002001.        
059700         05  FILLER       PIC 9(18)   VALUE    491005002002002001.        
059800         05  FILLER       PIC 9(18)   VALUE    492005002002002001.        
059900         05  FILLER       PIC 9(18)   VALUE    493005002002002001.        
060000         05  FILLER       PIC 9(18)   VALUE    494005002002002001.        
060100         05  FILLER       PIC 9(18)   VALUE    495005002002002001.        
060200         05  FILLER       PIC 9(18)   VALUE    496005002002002001.        
060300         05  FILLER       PIC 9(18)   VALUE    497005002002002001.        
060400         05  FILLER       PIC 9(18)   VALUE    498005002002002001.        
060500         05  FILLER       PIC 9(18)   VALUE    499005002002002001.        
060510*        05  ******      OVANFÖR FINNS ANSKAFFARE 499 *****               
060600         05  FILLER       PIC 9(18)   VALUE    500005002002002001.        
060800         05  FILLER       PIC 9(18)   VALUE    501005002002002001.        
060900         05  FILLER       PIC 9(18)   VALUE    502005002002002001.        
061000         05  FILLER       PIC 9(18)   VALUE    503005002002002001.        
061100         05  FILLER       PIC 9(18)   VALUE    504005002002002001.        
061200         05  FILLER       PIC 9(18)   VALUE    505005002002002001.        
061300         05  FILLER       PIC 9(18)   VALUE    506005002002002001.        
061400         05  FILLER       PIC 9(18)   VALUE    507005002002002001.        
061500         05  FILLER       PIC 9(18)   VALUE    508005002002002001.        
061600         05  FILLER       PIC 9(18)   VALUE    509005002002002001.        
061610*        05  ******      OVANFÖR FINNS ANSKAFFARE 509 *****               
061700         05  FILLER       PIC 9(18)   VALUE    510005002002002001.        
061900         05  FILLER       PIC 9(18)   VALUE    511005002002002001.        
062000         05  FILLER       PIC 9(18)   VALUE    512005002002002001.        
062100         05  FILLER       PIC 9(18)   VALUE    513005002002002001.        
062200         05  FILLER       PIC 9(18)   VALUE    514005002002002001.        
062300         05  FILLER       PIC 9(18)   VALUE    515005002002002001.        
062400         05  FILLER       PIC 9(18)   VALUE    516005002002002001.        
062500         05  FILLER       PIC 9(18)   VALUE    517005002002002001.        
062600         05  FILLER       PIC 9(18)   VALUE    518005002002002001.        
062700         05  FILLER       PIC 9(18)   VALUE    519005002002002001.        
062710*        05  ******      OVANFÖR FINNS ANSKAFFARE 519 *****               
062800         05  FILLER       PIC 9(18)   VALUE    520005002002002001.        
063000         05  FILLER       PIC 9(18)   VALUE    521005002002002001.        
063100         05  FILLER       PIC 9(18)   VALUE    522005002002002001.        
063200         05  FILLER       PIC 9(18)   VALUE    523005002002002001.        
063300         05  FILLER       PIC 9(18)   VALUE    524005002002002001.        
063400         05  FILLER       PIC 9(18)   VALUE    525005002002002001.        
063500         05  FILLER       PIC 9(18)   VALUE    526005002002002001.        
063600         05  FILLER       PIC 9(18)   VALUE    527005002002002001.        
063700         05  FILLER       PIC 9(18)   VALUE    528005002002002001.        
063800         05  FILLER       PIC 9(18)   VALUE    529005002002002001.        
063810*        05  ******      OVANFÖR FINNS ANSKAFFARE 529 *****               
063900         05  FILLER       PIC 9(18)   VALUE    530005002002002001.        
064100         05  FILLER       PIC 9(18)   VALUE    531005002002002001.        
064200         05  FILLER       PIC 9(18)   VALUE    532005002002002001.        
064300         05  FILLER       PIC 9(18)   VALUE    533005002002002001.        
064400         05  FILLER       PIC 9(18)   VALUE    534005002002002001.        
064500         05  FILLER       PIC 9(18)   VALUE    535005002002002001.        
064600         05  FILLER       PIC 9(18)   VALUE    536005002002002001.        
064700         05  FILLER       PIC 9(18)   VALUE    537005002002002001.        
064800         05  FILLER       PIC 9(18)   VALUE    538005002002002001.        
064900         05  FILLER       PIC 9(18)   VALUE    539005002002002001.        
064910*        05  ******      OVANFÖR FINNS ANSKAFFARE 539 *****               
065000         05  FILLER       PIC 9(18)   VALUE    540005002002002001.        
065200         05  FILLER       PIC 9(18)   VALUE    541005002002002001.        
065300         05  FILLER       PIC 9(18)   VALUE    542005002002002001.        
065400         05  FILLER       PIC 9(18)   VALUE    543005002002002001.        
065500         05  FILLER       PIC 9(18)   VALUE    544005002002002001.        
065600         05  FILLER       PIC 9(18)   VALUE    545005002002002001.        
065700         05  FILLER       PIC 9(18)   VALUE    546005002002002001.        
065800         05  FILLER       PIC 9(18)   VALUE    547005002002002001.        
065900         05  FILLER       PIC 9(18)   VALUE    548005002002002001.        
066000         05  FILLER       PIC 9(18)   VALUE    549005002002002001.        
066010*        05  ******      OVANFÖR FINNS ANSKAFFARE 549 *****               
066100         05  FILLER       PIC 9(18)   VALUE    550005002002002001.        
066300         05  FILLER       PIC 9(18)   VALUE    551005002002002001.        
066400         05  FILLER       PIC 9(18)   VALUE    552005002002002001.        
066500         05  FILLER       PIC 9(18)   VALUE    553005002002002001.        
066600         05  FILLER       PIC 9(18)   VALUE    554005002002002001.        
066700         05  FILLER       PIC 9(18)   VALUE    555005002002002001.        
066800         05  FILLER       PIC 9(18)   VALUE    556005002002002001.        
066900         05  FILLER       PIC 9(18)   VALUE    557005002002002001.        
067000         05  FILLER       PIC 9(18)   VALUE    558005002002002001.        
067100         05  FILLER       PIC 9(18)   VALUE    559005002002002001.        
067110*        05  ******      OVANFÖR FINNS ANSKAFFARE 559 *****               
067200         05  FILLER       PIC 9(18)   VALUE    560005002002002001.        
067400         05  FILLER       PIC 9(18)   VALUE    561005002002002001.        
067500         05  FILLER       PIC 9(18)   VALUE    562005002002002001.        
067600         05  FILLER       PIC 9(18)   VALUE    563005002002002001.        
067700         05  FILLER       PIC 9(18)   VALUE    564005002002002001.        
067800         05  FILLER       PIC 9(18)   VALUE    565005002002002001.        
067900         05  FILLER       PIC 9(18)   VALUE    566005002002002001.        
068000         05  FILLER       PIC 9(18)   VALUE    567005002002002001.        
068100         05  FILLER       PIC 9(18)   VALUE    568005002002002001.        
068200         05  FILLER       PIC 9(18)   VALUE    569005002002002001.        
068210*        05  ******      OVANFÖR FINNS ANSKAFFARE 569 *****               
068300         05  FILLER       PIC 9(18)   VALUE    570005002002002001.        
068500         05  FILLER       PIC 9(18)   VALUE    571005002002002001.        
068600         05  FILLER       PIC 9(18)   VALUE    572005002002002001.        
068700         05  FILLER       PIC 9(18)   VALUE    573005002002002001.        
068800         05  FILLER       PIC 9(18)   VALUE    574005002002002001.        
068900         05  FILLER       PIC 9(18)   VALUE    575005002002002001.        
069000         05  FILLER       PIC 9(18)   VALUE    576005002002002001.        
069100         05  FILLER       PIC 9(18)   VALUE    577005002002002001.        
069200         05  FILLER       PIC 9(18)   VALUE    578005002002002001.        
069300         05  FILLER       PIC 9(18)   VALUE    579005002002002001.        
069310*        05  ******      OVANFÖR FINNS ANSKAFFARE 579 *****               
069400         05  FILLER       PIC 9(18)   VALUE    580005002002002001.        
069600         05  FILLER       PIC 9(18)   VALUE    581005002002002001.        
069700         05  FILLER       PIC 9(18)   VALUE    582005002002002001.        
069800         05  FILLER       PIC 9(18)   VALUE    583005002002002001.        
069900         05  FILLER       PIC 9(18)   VALUE    584005002002002001.        
070000         05  FILLER       PIC 9(18)   VALUE    585005002002002001.        
070100         05  FILLER       PIC 9(18)   VALUE    586005002002002001.        
070200         05  FILLER       PIC 9(18)   VALUE    587005002002002001.        
070300         05  FILLER       PIC 9(18)   VALUE    588005002002002001.        
070400         05  FILLER       PIC 9(18)   VALUE    589005002002002001.        
070410*        05  ******      OVANFÖR FINNS ANSKAFFARE 589 *****               
070500         05  FILLER       PIC 9(18)   VALUE    590005002002002001.        
070700         05  FILLER       PIC 9(18)   VALUE    591005002002002001.        
070800         05  FILLER       PIC 9(18)   VALUE    592005002002002001.        
070900         05  FILLER       PIC 9(18)   VALUE    593005002002002001.        
071000         05  FILLER       PIC 9(18)   VALUE    594005002002002001.        
071100         05  FILLER       PIC 9(18)   VALUE    595005002002002001.        
071200         05  FILLER       PIC 9(18)   VALUE    596005002002002001.        
071300         05  FILLER       PIC 9(18)   VALUE    597005002002002001.        
071400         05  FILLER       PIC 9(18)   VALUE    598005002002002001.        
071500         05  FILLER       PIC 9(18)   VALUE    599005002002002001.        
071510*        05  ******      OVANFÖR FINNS ANSKAFFARE 599 *****               
071600         05  FILLER       PIC 9(18)   VALUE    600006002002002001.        
071800         05  FILLER       PIC 9(18)   VALUE    601006002002002001.        
071900         05  FILLER       PIC 9(18)   VALUE    602006002002002001.        
072000         05  FILLER       PIC 9(18)   VALUE    603006002002002001.        
072100         05  FILLER       PIC 9(18)   VALUE    604006002002002001.        
072200         05  FILLER       PIC 9(18)   VALUE    605006002002002001.        
072300         05  FILLER       PIC 9(18)   VALUE    606006002002002001.        
072400         05  FILLER       PIC 9(18)   VALUE    607006002002002001.        
072500         05  FILLER       PIC 9(18)   VALUE    608006002002002001.        
072600         05  FILLER       PIC 9(18)   VALUE    609006002002002001.        
072610*        05  ******      OVANFÖR FINNS ANSKAFFARE 609 *****               
072700         05  FILLER       PIC 9(18)   VALUE    610006002002002001.        
072900         05  FILLER       PIC 9(18)   VALUE    611006002002002001.        
073000         05  FILLER       PIC 9(18)   VALUE    612006002002002001.        
073100         05  FILLER       PIC 9(18)   VALUE    613006002002002001.        
073200         05  FILLER       PIC 9(18)   VALUE    614006002002002001.        
073300         05  FILLER       PIC 9(18)   VALUE    615006002002002001.        
073400         05  FILLER       PIC 9(18)   VALUE    616006002002002001.        
073500         05  FILLER       PIC 9(18)   VALUE    617006002002002001.        
073600         05  FILLER       PIC 9(18)   VALUE    618006002002002001.        
073700         05  FILLER       PIC 9(18)   VALUE    619006002002002001.        
073710*        05  ******      OVANFÖR FINNS ANSKAFFARE 619 *****               
073800         05  FILLER       PIC 9(18)   VALUE    620006002002002001.        
074000         05  FILLER       PIC 9(18)   VALUE    621006002002002001.        
074100         05  FILLER       PIC 9(18)   VALUE    622006002002002001.        
074200         05  FILLER       PIC 9(18)   VALUE    623006002002002001.        
074300         05  FILLER       PIC 9(18)   VALUE    624006002002002001.        
074400         05  FILLER       PIC 9(18)   VALUE    625006002002002001.        
074500         05  FILLER       PIC 9(18)   VALUE    626006002002002001.        
074600         05  FILLER       PIC 9(18)   VALUE    627006002002002001.        
074700         05  FILLER       PIC 9(18)   VALUE    628006002002002001.        
074800         05  FILLER       PIC 9(18)   VALUE    629006002002002001.        
074810*        05  ******      OVANFÖR FINNS ANSKAFFARE 629 *****               
074900         05  FILLER       PIC 9(18)   VALUE    630006002002002001.        
075100         05  FILLER       PIC 9(18)   VALUE    631006002002002001.        
075200         05  FILLER       PIC 9(18)   VALUE    632006002002002001.        
075300         05  FILLER       PIC 9(18)   VALUE    633006002002002001.        
075400         05  FILLER       PIC 9(18)   VALUE    634006002002002001.        
075500         05  FILLER       PIC 9(18)   VALUE    635006002002002001.        
075600         05  FILLER       PIC 9(18)   VALUE    636006002002002001.        
075700         05  FILLER       PIC 9(18)   VALUE    637006002002002001.        
075800         05  FILLER       PIC 9(18)   VALUE    638006002002002001.        
075900         05  FILLER       PIC 9(18)   VALUE    639006002002002001.        
075910*        05  ******      OVANFÖR FINNS ANSKAFFARE 639 *****               
076000         05  FILLER       PIC 9(18)   VALUE    640006002002002001.        
076200         05  FILLER       PIC 9(18)   VALUE    641006002002002001.        
076300         05  FILLER       PIC 9(18)   VALUE    642006002002002001.        
076400         05  FILLER       PIC 9(18)   VALUE    643006002002002001.        
076500         05  FILLER       PIC 9(18)   VALUE    644006002002002001.        
076600         05  FILLER       PIC 9(18)   VALUE    645006002002002001.        
076700         05  FILLER       PIC 9(18)   VALUE    646006002002002001.        
076800         05  FILLER       PIC 9(18)   VALUE    647006002002002001.        
076900         05  FILLER       PIC 9(18)   VALUE    648006002002002001.        
077000         05  FILLER       PIC 9(18)   VALUE    649006002002002001.        
077010*        05  ******      OVANFÖR FINNS ANSKAFFARE 649 *****               
077100         05  FILLER       PIC 9(18)   VALUE    650006002002002001.        
077300         05  FILLER       PIC 9(18)   VALUE    651006002002002001.        
077400         05  FILLER       PIC 9(18)   VALUE    652006002002002001.        
077500         05  FILLER       PIC 9(18)   VALUE    653006002002002001.        
077600         05  FILLER       PIC 9(18)   VALUE    654006002002002001.        
077700         05  FILLER       PIC 9(18)   VALUE    655006002002002001.        
077800         05  FILLER       PIC 9(18)   VALUE    656006002002002001.        
077900         05  FILLER       PIC 9(18)   VALUE    657006002002002001.        
078000         05  FILLER       PIC 9(18)   VALUE    658006002002002001.        
078100         05  FILLER       PIC 9(18)   VALUE    659006002002002001.        
078110*        05  ******      OVANFÖR FINNS ANSKAFFARE 659 *****               
078200         05  FILLER       PIC 9(18)   VALUE    660006002002002001.        
078400         05  FILLER       PIC 9(18)   VALUE    661006002002002001.        
078500         05  FILLER       PIC 9(18)   VALUE    662006002002002001.        
078600         05  FILLER       PIC 9(18)   VALUE    663006002002002001.        
078700         05  FILLER       PIC 9(18)   VALUE    664006002002002001.        
078800         05  FILLER       PIC 9(18)   VALUE    665006002002002001.        
078900         05  FILLER       PIC 9(18)   VALUE    666006002002002001.        
079000         05  FILLER       PIC 9(18)   VALUE    667006002002002001.        
079100         05  FILLER       PIC 9(18)   VALUE    668006002002002001.        
079200         05  FILLER       PIC 9(18)   VALUE    669006002002002001.        
079210*        05  ******      OVANFÖR FINNS ANSKAFFARE 669 *****               
079300         05  FILLER       PIC 9(18)   VALUE    670006002002002001.        
079500         05  FILLER       PIC 9(18)   VALUE    671006002002002001.        
079600         05  FILLER       PIC 9(18)   VALUE    672006002002002001.        
079700         05  FILLER       PIC 9(18)   VALUE    673006002002002001.        
079800         05  FILLER       PIC 9(18)   VALUE    674006002002002001.        
079900         05  FILLER       PIC 9(18)   VALUE    675006002002002001.        
080000         05  FILLER       PIC 9(18)   VALUE    676006002002002001.        
080100         05  FILLER       PIC 9(18)   VALUE    677006002002002001.        
080200         05  FILLER       PIC 9(18)   VALUE    678006002002002001.        
080300         05  FILLER       PIC 9(18)   VALUE    679006002002002001.        
080310*        05  ******      OVANFÖR FINNS ANSKAFFARE 679 *****               
080400         05  FILLER       PIC 9(18)   VALUE    680006002002002001.        
080600         05  FILLER       PIC 9(18)   VALUE    681006002002002001.        
080700         05  FILLER       PIC 9(18)   VALUE    682006002002002001.        
080800         05  FILLER       PIC 9(18)   VALUE    683006002002002001.        
080900         05  FILLER       PIC 9(18)   VALUE    684006002002002001.        
081000         05  FILLER       PIC 9(18)   VALUE    685006002002002001.        
081100         05  FILLER       PIC 9(18)   VALUE    686006002002002001.        
081200         05  FILLER       PIC 9(18)   VALUE    687006002002002001.        
081300         05  FILLER       PIC 9(18)   VALUE    688006002002002001.        
081400         05  FILLER       PIC 9(18)   VALUE    689006002002002001.        
081410*        05  ******      OVANFÖR FINNS ANSKAFFARE 689 *****               
081500         05  FILLER       PIC 9(18)   VALUE    690006002002002001.        
081700         05  FILLER       PIC 9(18)   VALUE    691006002002002001.        
081800         05  FILLER       PIC 9(18)   VALUE    692006002002002001.        
081900         05  FILLER       PIC 9(18)   VALUE    693006002002002001.        
082000         05  FILLER       PIC 9(18)   VALUE    694006002002002001.        
082100         05  FILLER       PIC 9(18)   VALUE    695006002002002001.        
082200         05  FILLER       PIC 9(18)   VALUE    696006002002002001.        
082300         05  FILLER       PIC 9(18)   VALUE    697006002002002001.        
082400         05  FILLER       PIC 9(18)   VALUE    698006002002002001.        
082500         05  FILLER       PIC 9(18)   VALUE    699006002002002001.        
082510*        05  ******      OVANFÖR FINNS ANSKAFFARE 699 *****               
082600         05  FILLER       PIC 9(18)   VALUE    700007002002002001.        
082800         05  FILLER       PIC 9(18)   VALUE    701007002002002001.        
082900         05  FILLER       PIC 9(18)   VALUE    702007002002002001.        
083000         05  FILLER       PIC 9(18)   VALUE    703007002002002001.        
083100         05  FILLER       PIC 9(18)   VALUE    704007002002002001.        
083200         05  FILLER       PIC 9(18)   VALUE    705007002002002001.        
083300         05  FILLER       PIC 9(18)   VALUE    706007002002002001.        
083400         05  FILLER       PIC 9(18)   VALUE    707007002002002001.        
083500         05  FILLER       PIC 9(18)   VALUE    708007002002002001.        
083600         05  FILLER       PIC 9(18)   VALUE    709007002002002001.        
083610*        05  ******      OVANFÖR FINNS ANSKAFFARE 709 *****               
083700         05  FILLER       PIC 9(18)   VALUE    710007002002002001.        
083900         05  FILLER       PIC 9(18)   VALUE    711007002002002001.        
084000         05  FILLER       PIC 9(18)   VALUE    712007002002002001.        
084100         05  FILLER       PIC 9(18)   VALUE    713007002002002001.        
084200         05  FILLER       PIC 9(18)   VALUE    714007002002002001.        
084300         05  FILLER       PIC 9(18)   VALUE    715007002002002001.        
084400         05  FILLER       PIC 9(18)   VALUE    716007002002002001.        
084500         05  FILLER       PIC 9(18)   VALUE    717007002002002001.        
084600         05  FILLER       PIC 9(18)   VALUE    718007002002002001.        
084700         05  FILLER       PIC 9(18)   VALUE    719007002002002001.        
084710*        05  ******      OVANFÖR FINNS ANSKAFFARE 719 *****               
084800         05  FILLER       PIC 9(18)   VALUE    720007002002002001.        
085000         05  FILLER       PIC 9(18)   VALUE    721007002002002001.        
085100         05  FILLER       PIC 9(18)   VALUE    722007002002002001.        
085200         05  FILLER       PIC 9(18)   VALUE    723007002002002001.        
085300         05  FILLER       PIC 9(18)   VALUE    724007002002002001.        
085400         05  FILLER       PIC 9(18)   VALUE    725007002002002001.        
085500         05  FILLER       PIC 9(18)   VALUE    726007002002002001.        
085600         05  FILLER       PIC 9(18)   VALUE    727007002002002001.        
085700         05  FILLER       PIC 9(18)   VALUE    728007002002002001.        
085800         05  FILLER       PIC 9(18)   VALUE    729007002002002001.        
085810*        05  ******      OVANFÖR FINNS ANSKAFFARE 729 *****               
085900         05  FILLER       PIC 9(18)   VALUE    730007002002002001.        
086100         05  FILLER       PIC 9(18)   VALUE    731007002002002001.        
086200         05  FILLER       PIC 9(18)   VALUE    732007002002002001.        
086300         05  FILLER       PIC 9(18)   VALUE    733007002002002001.        
086400         05  FILLER       PIC 9(18)   VALUE    734007002002002001.        
086500         05  FILLER       PIC 9(18)   VALUE    735007002002002001.        
086600         05  FILLER       PIC 9(18)   VALUE    736007002002002001.        
086700         05  FILLER       PIC 9(18)   VALUE    737007002002002001.        
086800         05  FILLER       PIC 9(18)   VALUE    738007002002002001.        
086900         05  FILLER       PIC 9(18)   VALUE    739007002002002001.        
086910*        05  ******      OVANFÖR FINNS ANSKAFFARE 739 *****               
087000         05  FILLER       PIC 9(18)   VALUE    740007002002002001.        
087200         05  FILLER       PIC 9(18)   VALUE    741007002002002001.        
087300         05  FILLER       PIC 9(18)   VALUE    742007002002002001.        
087400         05  FILLER       PIC 9(18)   VALUE    743007002002002001.        
087500         05  FILLER       PIC 9(18)   VALUE    744007002002002001.        
087600         05  FILLER       PIC 9(18)   VALUE    745007002002002001.        
087700         05  FILLER       PIC 9(18)   VALUE    746007002002002001.        
087800         05  FILLER       PIC 9(18)   VALUE    747007002002002001.        
087900         05  FILLER       PIC 9(18)   VALUE    748007002002002001.        
088000         05  FILLER       PIC 9(18)   VALUE    749007002002002001.        
088010*        05  ******      OVANFÖR FINNS ANSKAFFARE 749 *****               
088100         05  FILLER       PIC 9(18)   VALUE    750007002002002001.        
088300         05  FILLER       PIC 9(18)   VALUE    751007002002002001.        
088400         05  FILLER       PIC 9(18)   VALUE    752007002002002001.        
088500         05  FILLER       PIC 9(18)   VALUE    753007002002002001.        
088600         05  FILLER       PIC 9(18)   VALUE    754007002002002001.        
088700         05  FILLER       PIC 9(18)   VALUE    755007002002002001.        
088800         05  FILLER       PIC 9(18)   VALUE    756007002002002001.        
088900         05  FILLER       PIC 9(18)   VALUE    757007002002002001.        
089000         05  FILLER       PIC 9(18)   VALUE    758007002002002001.        
089100         05  FILLER       PIC 9(18)   VALUE    759007002002002001.        
089110*        05  ******      OVANFÖR FINNS ANSKAFFARE 759 *****               
089200         05  FILLER       PIC 9(18)   VALUE    760007002002002001.        
089400         05  FILLER       PIC 9(18)   VALUE    761007002002002001.        
089500         05  FILLER       PIC 9(18)   VALUE    762007002002002001.        
089600         05  FILLER       PIC 9(18)   VALUE    763007002002002001.        
089700         05  FILLER       PIC 9(18)   VALUE    764007002002002001.        
089800         05  FILLER       PIC 9(18)   VALUE    765007002002002001.        
089900         05  FILLER       PIC 9(18)   VALUE    766007002002002001.        
090000         05  FILLER       PIC 9(18)   VALUE    767007002002002001.        
090100         05  FILLER       PIC 9(18)   VALUE    768007002002002001.        
090200         05  FILLER       PIC 9(18)   VALUE    769007002002002001.        
090210*        05  ******      OVANFÖR FINNS ANSKAFFARE 769 *****               
090300         05  FILLER       PIC 9(18)   VALUE    770007002002002001.        
090500         05  FILLER       PIC 9(18)   VALUE    771007002002002001.        
090600         05  FILLER       PIC 9(18)   VALUE    772007002002002001.        
090700         05  FILLER       PIC 9(18)   VALUE    773007002002002001.        
090800         05  FILLER       PIC 9(18)   VALUE    774007002002002001.        
090900         05  FILLER       PIC 9(18)   VALUE    775007002002002001.        
091000         05  FILLER       PIC 9(18)   VALUE    776007002002002001.        
091100         05  FILLER       PIC 9(18)   VALUE    777007002002002001.        
091200         05  FILLER       PIC 9(18)   VALUE    778007002002002001.        
091300         05  FILLER       PIC 9(18)   VALUE    779007002002002001.        
091310*        05  ******      OVANFÖR FINNS ANSKAFFARE 779 *****               
091400         05  FILLER       PIC 9(18)   VALUE    780007002002002001.        
091600         05  FILLER       PIC 9(18)   VALUE    781007002002002001.        
091700         05  FILLER       PIC 9(18)   VALUE    782007002002002001.        
091800         05  FILLER       PIC 9(18)   VALUE    783007002002002001.        
091900         05  FILLER       PIC 9(18)   VALUE    784007002002002001.        
092000         05  FILLER       PIC 9(18)   VALUE    785007002002002001.        
092100         05  FILLER       PIC 9(18)   VALUE    786007002002002001.        
092200         05  FILLER       PIC 9(18)   VALUE    787007002002002001.        
092300         05  FILLER       PIC 9(18)   VALUE    788007002002002001.        
092400         05  FILLER       PIC 9(18)   VALUE    789007002002002001.        
092410*        05  ******      OVANFÖR FINNS ANSKAFFARE 789 *****               
092500         05  FILLER       PIC 9(18)   VALUE    790007002002002001.        
092700         05  FILLER       PIC 9(18)   VALUE    791007002002002001.        
092800         05  FILLER       PIC 9(18)   VALUE    792007002002002001.        
092900         05  FILLER       PIC 9(18)   VALUE    793007002002002001.        
093000         05  FILLER       PIC 9(18)   VALUE    794007002002002001.        
093100         05  FILLER       PIC 9(18)   VALUE    795007002002002001.        
093200         05  FILLER       PIC 9(18)   VALUE    796007002002002001.        
093300         05  FILLER       PIC 9(18)   VALUE    797007002002002001.        
093400         05  FILLER       PIC 9(18)   VALUE    798007002002002001.        
093500         05  FILLER       PIC 9(18)   VALUE    799007002002002001.        
093510*        05  ******      OVANFÖR FINNS ANSKAFFARE 799 *****               
093600         05  FILLER       PIC 9(18)   VALUE    800008002002002001.        
093800         05  FILLER       PIC 9(18)   VALUE    801008002002002001.        
093900         05  FILLER       PIC 9(18)   VALUE    802008002002002001.        
094000         05  FILLER       PIC 9(18)   VALUE    803008002002002001.        
094100         05  FILLER       PIC 9(18)   VALUE    804008002002002001.        
094200         05  FILLER       PIC 9(18)   VALUE    805008002002002001.        
094300         05  FILLER       PIC 9(18)   VALUE    806008002002002001.        
094400         05  FILLER       PIC 9(18)   VALUE    807008002002002001.        
094500         05  FILLER       PIC 9(18)   VALUE    808008002002002001.        
094600         05  FILLER       PIC 9(18)   VALUE    809008002002002001.        
094610*        05  ******      OVANFÖR FINNS ANSKAFFARE 809 *****               
094700         05  FILLER       PIC 9(18)   VALUE    810008002002002001.        
094900         05  FILLER       PIC 9(18)   VALUE    811008002002002001.        
095000         05  FILLER       PIC 9(18)   VALUE    812008002002002001.        
095100         05  FILLER       PIC 9(18)   VALUE    813008002002002001.        
095200         05  FILLER       PIC 9(18)   VALUE    814008002002002001.        
095300         05  FILLER       PIC 9(18)   VALUE    815008002002002001.        
095400         05  FILLER       PIC 9(18)   VALUE    816008002002002001.        
095500         05  FILLER       PIC 9(18)   VALUE    817008002002002001.        
095600         05  FILLER       PIC 9(18)   VALUE    818008002002002001.        
095700         05  FILLER       PIC 9(18)   VALUE    819008002002002001.        
095710*        05  ******      OVANFÖR FINNS ANSKAFFARE 819 *****               
095800         05  FILLER       PIC 9(18)   VALUE    820008002002002001.        
096000         05  FILLER       PIC 9(18)   VALUE    821008002002002001.        
096100         05  FILLER       PIC 9(18)   VALUE    822008002002002001.        
096200         05  FILLER       PIC 9(18)   VALUE    823008002002002001.        
096300         05  FILLER       PIC 9(18)   VALUE    824008002002002001.        
096400         05  FILLER       PIC 9(18)   VALUE    825008002002002001.        
096500         05  FILLER       PIC 9(18)   VALUE    826008002002002001.        
096600         05  FILLER       PIC 9(18)   VALUE    827008002002002001.        
096700         05  FILLER       PIC 9(18)   VALUE    828008002002002001.        
096800         05  FILLER       PIC 9(18)   VALUE    829008002002002001.        
096810*        05  ******      OVANFÖR FINNS ANSKAFFARE 829 *****               
096900         05  FILLER       PIC 9(18)   VALUE    830008002002002001.        
097100         05  FILLER       PIC 9(18)   VALUE    831008002002002001.        
097200         05  FILLER       PIC 9(18)   VALUE    832008002002002001.        
097300         05  FILLER       PIC 9(18)   VALUE    833008002002002001.        
097400         05  FILLER       PIC 9(18)   VALUE    834008002002002001.        
097500         05  FILLER       PIC 9(18)   VALUE    835008002002002001.        
097600         05  FILLER       PIC 9(18)   VALUE    836008002002002001.        
097700         05  FILLER       PIC 9(18)   VALUE    837008002002002001.        
097800         05  FILLER       PIC 9(18)   VALUE    838008002002002001.        
097900         05  FILLER       PIC 9(18)   VALUE    839008002002002001.        
097910*        05  ******      OVANFÖR FINNS ANSKAFFARE 839 *****               
098000         05  FILLER       PIC 9(18)   VALUE    840008002002002001.        
098200         05  FILLER       PIC 9(18)   VALUE    841008002002002001.        
098300         05  FILLER       PIC 9(18)   VALUE    842008002002002001.        
098400         05  FILLER       PIC 9(18)   VALUE    843008002002002001.        
098500         05  FILLER       PIC 9(18)   VALUE    844008002002002001.        
098600         05  FILLER       PIC 9(18)   VALUE    845008002002002001.        
098700         05  FILLER       PIC 9(18)   VALUE    846008002002002001.        
098800         05  FILLER       PIC 9(18)   VALUE    847008002002002001.        
098900         05  FILLER       PIC 9(18)   VALUE    848008002002002001.        
099000         05  FILLER       PIC 9(18)   VALUE    849008002002002001.        
099010*        05  ******      OVANFÖR FINNS ANSKAFFARE 849 *****               
099100         05  FILLER       PIC 9(18)   VALUE    850008002002002001.        
099300         05  FILLER       PIC 9(18)   VALUE    851008002002002001.        
099400         05  FILLER       PIC 9(18)   VALUE    852008002002002001.        
099500         05  FILLER       PIC 9(18)   VALUE    853008002002002001.        
099600         05  FILLER       PIC 9(18)   VALUE    854008002002002001.        
099700         05  FILLER       PIC 9(18)   VALUE    855008002002002001.        
099800         05  FILLER       PIC 9(18)   VALUE    856008002002002001.        
099900         05  FILLER       PIC 9(18)   VALUE    857008002002002001.        
100000         05  FILLER       PIC 9(18)   VALUE    858008002002002001.        
100100         05  FILLER       PIC 9(18)   VALUE    859008002002002001.        
100110*        05  ******      OVANFÖR FINNS ANSKAFFARE 859 *****               
100200         05  FILLER       PIC 9(18)   VALUE    860008002002002001.        
100400         05  FILLER       PIC 9(18)   VALUE    861008002002002001.        
100500         05  FILLER       PIC 9(18)   VALUE    862008002002002001.        
100600         05  FILLER       PIC 9(18)   VALUE    863008002002002001.        
100700         05  FILLER       PIC 9(18)   VALUE    864008002002002001.        
100800         05  FILLER       PIC 9(18)   VALUE    865008002002002001.        
100900         05  FILLER       PIC 9(18)   VALUE    866008002002002001.        
101000         05  FILLER       PIC 9(18)   VALUE    867008002002002001.        
101100         05  FILLER       PIC 9(18)   VALUE    868008002002002001.        
101200         05  FILLER       PIC 9(18)   VALUE    869008002002002001.        
101210*        05  ******      OVANFÖR FINNS ANSKAFFARE 869 *****               
101300         05  FILLER       PIC 9(18)   VALUE    870008002002002001.        
101500         05  FILLER       PIC 9(18)   VALUE    871008002002002001.        
101600         05  FILLER       PIC 9(18)   VALUE    872008002002002001.        
101700         05  FILLER       PIC 9(18)   VALUE    873008002002002001.        
101800         05  FILLER       PIC 9(18)   VALUE    874008002002002001.        
101900         05  FILLER       PIC 9(18)   VALUE    875008002002002001.        
102000         05  FILLER       PIC 9(18)   VALUE    876008002002002001.        
102100         05  FILLER       PIC 9(18)   VALUE    877008002002002001.        
102200         05  FILLER       PIC 9(18)   VALUE    878008002002002001.        
102300         05  FILLER       PIC 9(18)   VALUE    879008002002002001.        
102310*        05  ******      OVANFÖR FINNS ANSKAFFARE 879 *****               
102400         05  FILLER       PIC 9(18)   VALUE    880008002002002001.        
102600         05  FILLER       PIC 9(18)   VALUE    881008002002002001.        
102700         05  FILLER       PIC 9(18)   VALUE    882008002002002001.        
102800         05  FILLER       PIC 9(18)   VALUE    883008002002002001.        
102900         05  FILLER       PIC 9(18)   VALUE    884008002002002001.        
103000         05  FILLER       PIC 9(18)   VALUE    885008002002002001.        
103100         05  FILLER       PIC 9(18)   VALUE    886008002002002001.        
103200         05  FILLER       PIC 9(18)   VALUE    887008002002002001.        
103300         05  FILLER       PIC 9(18)   VALUE    988008002002002001.        
103400         05  FILLER       PIC 9(18)   VALUE    889008002002002001.        
103410*        05  ******      OVANFÖR FINNS ANSKAFFARE 889 *****               
103500         05  FILLER       PIC 9(18)   VALUE    890008002002002001.        
103700         05  FILLER       PIC 9(18)   VALUE    891008002002002001.        
103800         05  FILLER       PIC 9(18)   VALUE    892008002002002001.        
103900         05  FILLER       PIC 9(18)   VALUE    893008002002002001.        
104000         05  FILLER       PIC 9(18)   VALUE    894008002002002001.        
104100         05  FILLER       PIC 9(18)   VALUE    895008002002002001.        
104200         05  FILLER       PIC 9(18)   VALUE    896008002002002001.        
104300         05  FILLER       PIC 9(18)   VALUE    897008002002002001.        
104400         05  FILLER       PIC 9(18)   VALUE    898008002002002001.        
104500         05  FILLER       PIC 9(18)   VALUE    899008002002002001.        
104510*        05  ******      OVANFÖR FINNS ANSKAFFARE 899 *****               
104600         05  FILLER       PIC 9(18)   VALUE    900009003003002001.        
104800         05  FILLER       PIC 9(18)   VALUE    901009003003002001.        
104900         05  FILLER       PIC 9(18)   VALUE    902009003003002001.        
105000         05  FILLER       PIC 9(18)   VALUE    903009003003002001.        
105100         05  FILLER       PIC 9(18)   VALUE    904009003003002001.        
105200         05  FILLER       PIC 9(18)   VALUE    905009003003002001.        
105300         05  FILLER       PIC 9(18)   VALUE    906009003003002001.        
105400         05  FILLER       PIC 9(18)   VALUE    907009003003002001.        
105500         05  FILLER       PIC 9(18)   VALUE    908009003003002001.        
105600         05  FILLER       PIC 9(18)   VALUE    909009003003002001.        
105610*        05  ******      OVANFÖR FINNS ANSKAFFARE 909 *****               
105700         05  FILLER       PIC 9(18)   VALUE    910010004003002001.        
105900         05  FILLER       PIC 9(18)   VALUE    911010004003002001.        
106000         05  FILLER       PIC 9(18)   VALUE    912010004003002001.        
106100         05  FILLER       PIC 9(18)   VALUE    913010004003002001.        
106200         05  FILLER       PIC 9(18)   VALUE    914010004003002001.        
106300         05  FILLER       PIC 9(18)   VALUE    915010004003002001.        
106400         05  FILLER       PIC 9(18)   VALUE    916010004003002001.        
106500         05  FILLER       PIC 9(18)   VALUE    917010004003002001.        
106600         05  FILLER       PIC 9(18)   VALUE    918010004003002001.        
106700         05  FILLER       PIC 9(18)   VALUE    919010004003002001.        
106710*        05  ******      OVANFÖR FINNS ANSKAFFARE 919 *****               
106800         05  FILLER       PIC 9(18)   VALUE    920011005004002001.        
107000         05  FILLER       PIC 9(18)   VALUE    921011005004002001.        
107100         05  FILLER       PIC 9(18)   VALUE    922011005004002001.        
107200         05  FILLER       PIC 9(18)   VALUE    923011005004002001.        
107300         05  FILLER       PIC 9(18)   VALUE    924011005004002001.        
107400         05  FILLER       PIC 9(18)   VALUE    925011005004002001.        
107500         05  FILLER       PIC 9(18)   VALUE    926011005004002001.        
107600         05  FILLER       PIC 9(18)   VALUE    927011005004002001.        
107700         05  FILLER       PIC 9(18)   VALUE    928011005004002001.        
107800         05  FILLER       PIC 9(18)   VALUE    929011005004002001.        
107810*        05  ******      OVANFÖR FINNS ANSKAFFARE 929 *****               
107900         05  FILLER       PIC 9(18)   VALUE    930012005004002001.        
108100         05  FILLER       PIC 9(18)   VALUE    931012005004002001.        
108200         05  FILLER       PIC 9(18)   VALUE    932012005004002001.        
108300         05  FILLER       PIC 9(18)   VALUE    933012005004002001.        
108400         05  FILLER       PIC 9(18)   VALUE    934012005004002001.        
108500         05  FILLER       PIC 9(18)   VALUE    935012005004002001.        
108600         05  FILLER       PIC 9(18)   VALUE    936012005004002001.        
108700         05  FILLER       PIC 9(18)   VALUE    937012005004002001.        
108800         05  FILLER       PIC 9(18)   VALUE    938012005004002001.        
108900         05  FILLER       PIC 9(18)   VALUE    939012005004002001.        
108910*        05  ******      OVANFÖR FINNS ANSKAFFARE 939 *****               
109000         05  FILLER       PIC 9(18)   VALUE    940013005004002001.        
109200         05  FILLER       PIC 9(18)   VALUE    941013005004002001.        
109300         05  FILLER       PIC 9(18)   VALUE    942013005004002001.        
109400         05  FILLER       PIC 9(18)   VALUE    943013005004002001.        
109500         05  FILLER       PIC 9(18)   VALUE    944013005004002001.        
109600         05  FILLER       PIC 9(18)   VALUE    945013005004002001.        
109700         05  FILLER       PIC 9(18)   VALUE    946013005004002001.        
109800         05  FILLER       PIC 9(18)   VALUE    947013005004002001.        
109900         05  FILLER       PIC 9(18)   VALUE    948013005004002001.        
110000         05  FILLER       PIC 9(18)   VALUE    949013005004002001.        
110010*        05  ******      OVANFÖR FINNS ANSKAFFARE 949 *****               
110100         05  FILLER       PIC 9(18)   VALUE    950014005004002001.        
110300         05  FILLER       PIC 9(18)   VALUE    951014005004002001.        
110400         05  FILLER       PIC 9(18)   VALUE    952014005004002001.        
110500         05  FILLER       PIC 9(18)   VALUE    953014005004002001.        
110600         05  FILLER       PIC 9(18)   VALUE    954014005004002001.        
110700         05  FILLER       PIC 9(18)   VALUE    955014005004002001.        
110800         05  FILLER       PIC 9(18)   VALUE    956014005004002001.        
110900         05  FILLER       PIC 9(18)   VALUE    957014005004002001.        
111000         05  FILLER       PIC 9(18)   VALUE    958014005004002001.        
111100         05  FILLER       PIC 9(18)   VALUE    959014005004002001.        
111110*        05  ******      OVANFÖR FINNS ANSKAFFARE 959 *****               
111200         05  FILLER       PIC 9(18)   VALUE    960015006005003001.        
111400         05  FILLER       PIC 9(18)   VALUE    961015006005003001.        
111500         05  FILLER       PIC 9(18)   VALUE    962015006005003001.        
111600         05  FILLER       PIC 9(18)   VALUE    963015006005003001.        
111700         05  FILLER       PIC 9(18)   VALUE    964015006005003001.        
111800         05  FILLER       PIC 9(18)   VALUE    965015006005003001.        
111900         05  FILLER       PIC 9(18)   VALUE    966015006005003001.        
112000         05  FILLER       PIC 9(18)   VALUE    967015006005003001.        
112100         05  FILLER       PIC 9(18)   VALUE    968015006005003001.        
112200         05  FILLER       PIC 9(18)   VALUE    969015006005003001.        
112210*        05  ******      OVANFÖR FINNS ANSKAFFARE 969 *****               
112300         05  FILLER       PIC 9(18)   VALUE    970016007006004001.        
112500         05  FILLER       PIC 9(18)   VALUE    971016007006004001.        
112600         05  FILLER       PIC 9(18)   VALUE    972016007006004001.        
112700         05  FILLER       PIC 9(18)   VALUE    973016007006004001.        
112800         05  FILLER       PIC 9(18)   VALUE    974016007006004001.        
112900         05  FILLER       PIC 9(18)   VALUE    975016007006004001.        
113000         05  FILLER       PIC 9(18)   VALUE    976016007006004001.        
113100         05  FILLER       PIC 9(18)   VALUE    977016007006004001.        
113200         05  FILLER       PIC 9(18)   VALUE    978016007006004001.        
113300         05  FILLER       PIC 9(18)   VALUE    979016007006004001.        
113310*        05  ******      OVANFÖR FINNS ANSKAFFARE 979 *****               
113400         05  FILLER       PIC 9(18)   VALUE    980016007006004001.        
113600         05  FILLER       PIC 9(18)   VALUE    981016007006004001.        
113700         05  FILLER       PIC 9(18)   VALUE    982016007006004001.        
113800         05  FILLER       PIC 9(18)   VALUE    983016007006004001.        
113900         05  FILLER       PIC 9(18)   VALUE    984016007006004001.        
114000         05  FILLER       PIC 9(18)   VALUE    985016007006004001.        
114100         05  FILLER       PIC 9(18)   VALUE    986016007006004001.        
114200         05  FILLER       PIC 9(18)   VALUE    987016007006004001.        
114300         05  FILLER       PIC 9(18)   VALUE    988016007006004001.        
114400         05  FILLER       PIC 9(18)   VALUE    989016007006004001.        
114410*        05  ******      OVANFÖR FINNS ANSKAFFARE 989 *****               
114500         05  FILLER       PIC 9(18)   VALUE    990016007006004001.        
114700         05  FILLER       PIC 9(18)   VALUE    991016007006004001.        
114800         05  FILLER       PIC 9(18)   VALUE    992016007006004001.        
114900         05  FILLER       PIC 9(18)   VALUE    993016007006004001.        
115000         05  FILLER       PIC 9(18)   VALUE    994016007006004001.        
115100         05  FILLER       PIC 9(18)   VALUE    995016007006004001.        
115200         05  FILLER       PIC 9(18)   VALUE    996016007006004001.        
115300         05  FILLER       PIC 9(18)   VALUE    997016007006004001.        
115400         05  FILLER       PIC 9(18)   VALUE    998016007006004001.        
115500         05  FILLER       PIC 9(18)   VALUE    999016007006004001.        
115600     03  FILLER REDEFINES TAB-STORLEK-INDEX.                              
115700         05  TAB-A-IX  OCCURS 1000 ASCENDING KEY ANSK-IX                  
115800             INDEXED BY IX.                                               
115900             07  ANSK-IX     PIC 9(3).                                    
116000             07  GRUPP-IX    PIC 9(3).                                    
116100             07  SEKTION-IX  PIC 9(3).                                    
116200             07  FUNKTION-IX PIC 9(3).                                    
116300             07  OMRADE-IX   PIC 9(3).                                    
116400             07  TOTAL-IX    PIC 9(3).                                    
116500     SKIP2                                                                
116600 01  FILLER                  PIC X(18)   VALUE 'TAB-INDEX-SLUT'.          
116700     EJECT                                                                
116800 01  FILLER                  PIC X(16)   VALUE 'TAB-STORLEK-TEXT'.        
116900 01  TABELL-ANSK-TEXT.                                                    
117000     03  TAB-STORLEK-TEXT.                                                
117100         05  FILLER          PIC X(29)                                    
117200             VALUE '0-99 S  0-99 F  0-99    TOTAL'.                       
118700         05  FILLER          PIC X(29)                                    
118800             VALUE 'RES PV  PARTS PVOMR PV  TOTAL'.                       
118900         05  FILLER          PIC X(29)                                    
119000             VALUE 'L-ROVER SV BIL  OMR PV  TOTAL'.                       
119100         05  FILLER          PIC X(29)                                    
119200             VALUE 'RENAULT SV BIL  OMR PV  TOTAL'.                       
119300         05  FILLER          PIC X(29)                                    
119400             VALUE 'TBH PV  TILLB PVOMR PV  TOTAL'.                       
119500         05  FILLER          PIC X(29)                                    
119600             VALUE 'FÖRP. S.FÖRP. F.FÖRP    TOTAL'.                       
119700         05  FILLER          PIC X(29)                                    
119800             VALUE 'VAK. S. VAK. F. VAKANT  TOTAL'.                       
119900     03  FILLER REDEFINES TAB-STORLEK-TEXT.                               
120000         05  TAB-T-IX  OCCURS 7.                                          
120100             07  SEKTION-TXT    PIC X(8).                                 
120200             07  FUNKTION-TXT   PIC X(8).                                 
120300             07  OMRADE-TXT     PIC X(8).                                 
120400             07  TOTAL-TXT      PIC X(5).                                 
120500 01  FILLER                  PIC X(16)  VALUE 'TAB-TEXT-SLUT'.            
120600     EJECT                                                                
120700 01  TABELL-INTERVALLER.                                                  
120800     03  TAB-STORLEK-INTERVALL.                                           
120900         05  FILLER          PIC X(28)                                    
121000             VALUE '000-049000-099000-099000-099'.                        
121100         05  FILLER          PIC X(28)                                    
121200             VALUE '050-099000-099000-099000-099'.                        
121300         05  FILLER          PIC X(28)                                    
121400             VALUE '100-299100-899100-899100-959'.                        
122100         05  FILLER          PIC X(28)                                    
122200             VALUE '300-399100-899100-899100-959'.                        
123100         05  FILLER          PIC X(28)                                    
123200             VALUE '400-599100-899100-899100-959'.                        
123300         05  FILLER          PIC X(28)                                    
123400             VALUE '600-699100-899100-899100-959'.                        
123500         05  FILLER          PIC X(28)                                    
123600             VALUE '700-799100-899100-899100-959'.                        
123700         05  FILLER          PIC X(28)                                    
123800             VALUE '800-899100-899100-899100-959'.                        
123900         05  FILLER          PIC X(28)                                    
124000             VALUE '900-909900-909900-919100-959'.                        
124100         05  FILLER          PIC X(28)                                    
124200             VALUE '910-919910-919900-919100-959'.                        
124300         05  FILLER          PIC X(28)                                    
124400             VALUE '920-929920-959920-959100-959'.                        
124500         05  FILLER          PIC X(28)                                    
124600             VALUE '930-939920-959920-959100-959'.                        
124700         05  FILLER          PIC X(28)                                    
124800             VALUE '940-949920-959920-959100-959'.                        
124900         05  FILLER          PIC X(28)                                    
125000             VALUE '950-959920-959920-959100-959'.                        
125100         05  FILLER          PIC X(28)                                    
125200             VALUE '960-969960-969960-969960-969'.                        
125300         05  FILLER          PIC X(28)                                    
125400             VALUE '970-999970-999970-999970-999'.                        
125500     03  FILLER REDEFINES TAB-STORLEK-INTERVALL.                          
125600         05  TAB-T-IX  OCCURS 16.                                         
125700             07  GRP-INTERVALL  PIC X(7).                                 
125800             07  SEKT-INTERVALL PIC X(7).                                 
125900             07  FUNK-INTERVALL PIC X(7).                                 
126000             07  OMR-INTERVALL  PIC X(7).                                 
126100 01  FILLER                  PIC X(16)  VALUE 'INTERVALL-SLUT'.           
126200     EJECT                                                                
126300 LINKAGE SECTION.                                                         
126400     SKIP2                                                                
126500*    -COPY W009W42   -PRE L-                                              
126700     EJECT                                                                
126800 PROCEDURE DIVISION USING L-W009W42.                                      
126900 MAIN SECTION.                                                            
127100                                                                          
127200     SET IX TO 1                                                          
127300     SEARCH ALL TAB-A-IX AT END                                           
127400              MOVE ZERO  TO L-IDGRUPP                                     
127500                            L-IDSEKT                                      
127600                            L-IDFUNK                                      
127700                            L-IDAFFOMR                                    
127800                            L-TOTAL-IX                                    
127900              MOVE SPACE TO L-TESEKT                                      
128000                            L-TEFUNK                                      
128100                            L-TEAFFOMR                                    
128200                            L-GRUPP-INTERVALL                             
128300                            L-SEKT-INTERVALL                              
128400                            L-FUNK-INTERVALL                              
128500                            L-OMR-INTERVALL                               
128600                            L-TOTAL-TXT                                   
128700                                                                          
128800       WHEN ANSK-IX (IX) = L-IDANSK                                       
128900         MOVE GRUPP-IX (IX) TO L-IDGRUPP                                  
129000         MOVE SEKTION-IX (IX) TO L-IDSEKT                                 
129100         MOVE FUNKTION-IX (IX) TO L-IDFUNK                                
129200         MOVE OMRADE-IX (IX) TO L-IDAFFOMR                                
129300         MOVE TOTAL-IX (IX) TO L-TOTAL-IX                                 
129400                                                                          
129500         MOVE SEKTION-TXT (L-IDSEKT) TO L-TESEKT                          
129600         MOVE FUNKTION-TXT (L-IDSEKT) TO L-TEFUNK                         
129700         MOVE OMRADE-TXT (L-IDSEKT) TO L-TEAFFOMR                         
129800         MOVE TOTAL-TXT (L-IDSEKT) TO L-TOTAL-TXT                         
129900         MOVE GRP-INTERVALL (L-IDGRUPP) TO L-GRUPP-INTERVALL              
130000         MOVE SEKT-INTERVALL (L-IDGRUPP) TO L-SEKT-INTERVALL              
130100         MOVE FUNK-INTERVALL (L-IDGRUPP) TO L-FUNK-INTERVALL              
130200         MOVE OMR-INTERVALL (L-IDGRUPP) TO L-OMR-INTERVALL                
130300     END-SEARCH                                                           
130400                                                                          
130500     MOVE ZERO TO RETURN-CODE                                             
130600     GOBACK                                                               
130700     .                                                                    
