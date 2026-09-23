000100*COMPOPT STDSUB=YES                                                       
000200                                                                          
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W517TRAD.                                                
000500 AUTHOR.         MARKUS ASPFJÄLL.                                         
000600 DATE-WRITTEN.   99/11/24.                                                
000700 DATE-COMPILED.                                                           
000800*                                                                         
000900*    FUNKTION: ANROP - "CALL W517TRAD USING TRAD-W517TRAD"                
001000*    BEHANDLING      - DENNA MODUL OMVANDLAR KDTRADP TILL                 
001100*                      ETT DISTRIKT (FÖR ATT SEDAN FÅ MARKNAD             
001200*                      I W517V1)                                          
001300*                                                                         
001400*        ÄNDRAD FÖR ETRACKER NO 1457678, INSTALLERAD 2004-10-07           
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700 DATA DIVISION.                                                           
001800     SKIP2                                                                
001900 WORKING-STORAGE SECTION.                                                 
002000                                                                          
002100                                                                          
002200*    -- CHECKED BY WY2000                                                 
002300 77  IDPGM                       PIC X(8)    VALUE 'W517TRAD'.            
002400 77  JA                          PIC X       VALUE 'J'.                   
002500 77  NEJ                         PIC X       VALUE 'N'.                   
002600*************************************************************             
002700* KDTRADP TABELL.                                                         
002800*                                                                         
002900*************************************************************             
003000 01   FILLER                    PIC X(16)  VALUE 'ALLA KDTRADP'.          
003100                                                                          
003200 01   TABELL.                                                             
003300     03  TRADINGPARTNERS.                                                 
003400        05 FILLER       PIC X(10) VALUE 'AE   05420'.                     
003500        05 FILLER       PIC X(10) VALUE 'AE01 08187'.                     
003600        05 FILLER       PIC X(10) VALUE 'AL   02635'.                     
003700        05 FILLER       PIC X(10) VALUE 'AN   07416'.                     
003800        05 FILLER       PIC X(10) VALUE 'AM   02665'.                     
003900        05 FILLER       PIC X(10) VALUE 'AO   03130'.                     
004000        05 FILLER       PIC X(10) VALUE 'AR   06310'.                     
004100        05 FILLER       PIC X(10) VALUE 'AR01 06310'.                     
004200        05 FILLER       PIC X(10) VALUE 'ARZ1 06310'.                     
004300        05 FILLER       PIC X(10) VALUE 'AT   02378'.                     
004400        05 FILLER       PIC X(10) VALUE 'AT01 02378'.                     
004500        05 FILLER       PIC X(10) VALUE 'AT05 02378'.                     
004600        05 FILLER       PIC X(10) VALUE 'AU01 07830'.                     
004700        05 FILLER       PIC X(10) VALUE 'AU13 07830'.                     
004800        05 FILLER       PIC X(10) VALUE 'AUMG 07871'.                     
004900        05 FILLER       PIC X(10) VALUE 'AZ   02665'.                     
005000        05 FILLER       PIC X(10) VALUE 'BD   06026'.                     
005100        05 FILLER       PIC X(10) VALUE 'BE   01279'.                     
005200        05 FILLER       PIC X(10) VALUE 'BE01 01258'.                     
005300        05 FILLER       PIC X(10) VALUE 'BE06 01280'.                     
005400        05 FILLER       PIC X(10) VALUE 'BE13 01257'.                     
005500        05 FILLER       PIC X(10) VALUE 'BEL1 01238'.                     
005600        05 FILLER       PIC X(10) VALUE 'BG   02560'.                     
005700        05 FILLER       PIC X(10) VALUE 'BH   06236'.                     
005800        05 FILLER       PIC X(10) VALUE 'BN   05625'.                     
005900        05 FILLER       PIC X(10) VALUE 'BO   07180'.                     
006000        05 FILLER       PIC X(10) VALUE 'BR01 07051'.                     
006100        05 FILLER       PIC X(10) VALUE 'BR12 07050'.                     
006200        05 FILLER       PIC X(10) VALUE 'BRZ1 07050'.                     
006300        05 FILLER       PIC X(10) VALUE 'BY   02643'.                     
006400        05 FILLER       PIC X(10) VALUE 'CA03 07674'.                     
006500        05 FILLER       PIC X(10) VALUE 'CH13 02078'.                     
006600        05 FILLER       PIC X(10) VALUE 'CI   02681'.                     
006700        05 FILLER       PIC X(10) VALUE 'CL   06480'.                     
006800        05 FILLER       PIC X(10) VALUE 'CN   06201'.                     
006900        05 FILLER       PIC X(10) VALUE 'CN05 06270'.                     
007000        05 FILLER       PIC X(10) VALUE 'CO   07482'.                     
007100        05 FILLER       PIC X(10) VALUE 'CR   07491'.                     
007200        05 FILLER       PIC X(10) VALUE 'CU   07434'.                     
007300        05 FILLER       PIC X(10) VALUE 'CY   06210'.                     
007400        05 FILLER       PIC X(10) VALUE 'CZ   02459'.                     
007500        05 FILLER       PIC X(10) VALUE 'CZ01 02365'.                     
007600        05 FILLER       PIC X(10) VALUE 'DE   02278'.                     
007700        05 FILLER       PIC X(10) VALUE 'DE01 02278'.                     
007800        05 FILLER       PIC X(10) VALUE 'DEZ1 02278'.                     
007900        05 FILLER       PIC X(10) VALUE 'DK   00999'.                     
008000        05 FILLER       PIC X(10) VALUE 'DK01 00978'.                     
008100        05 FILLER       PIC X(10) VALUE 'DO   07496'.                     
008200        05 FILLER       PIC X(10) VALUE 'DC   07485'.                     
008300        05 FILLER       PIC X(10) VALUE 'EC   07480'.                     
008400        05 FILLER       PIC X(10) VALUE 'EE   02630'.                     
008500        05 FILLER       PIC X(10) VALUE 'EG   03250'.                     
008600        05 FILLER       PIC X(10) VALUE 'ES   02178'.                     
008700        05 FILLER       PIC X(10) VALUE 'ES01 02178'.                     
008800        05 FILLER       PIC X(10) VALUE 'ESL1 02138'.                     
008900        05 FILLER       PIC X(10) VALUE 'ET   04611'.                     
009000        05 FILLER       PIC X(10) VALUE 'FI   01090'.                     
009100        05 FILLER       PIC X(10) VALUE 'FI01 01090'.                     
009200        05 FILLER       PIC X(10) VALUE 'FJ   05922'.                     
009300        05 FILLER       PIC X(10) VALUE 'FR   01479'.                     
009400        05 FILLER       PIC X(10) VALUE 'FR01 01478'.                     
009500        05 FILLER       PIC X(10) VALUE 'FRL1 01438'.                     
009600        05 FILLER       PIC X(10) VALUE 'GB   01379'.                     
009700        05 FILLER       PIC X(10) VALUE 'GB01 01348'.                     
009800        05 FILLER       PIC X(10) VALUE 'GBZ8 01348'.                     
009900        05 FILLER       PIC X(10) VALUE 'GE   02688'.                     
010000        05 FILLER       PIC X(10) VALUE 'GH   04550'.                     
010100        05 FILLER       PIC X(10) VALUE 'GP   07440'.                     
010200        05 FILLER       PIC X(10) VALUE 'GR   01558'.                     
010300        05 FILLER       PIC X(10) VALUE 'GR01 01558'.                     
010400        05 FILLER       PIC X(10) VALUE 'GRZ1 01558'.                     
010500        05 FILLER       PIC X(10) VALUE 'GT   07497'.                     
010600        05 FILLER       PIC X(10) VALUE 'GY   07060'.                     
010700        05 FILLER       PIC X(10) VALUE 'HK   06211'.                     
010800        05 FILLER       PIC X(10) VALUE 'HN   07390'.                     
010900        05 FILLER       PIC X(10) VALUE 'HU01 02374'.                     
011000        05 FILLER       PIC X(10) VALUE 'ID   05916'.                     
011100        05 FILLER       PIC X(10) VALUE 'IE01 01778'.                     
011200        05 FILLER       PIC X(10) VALUE 'IL   05120'.                     
011300        05 FILLER       PIC X(10) VALUE 'IN   08167'.                     
011400        05 FILLER       PIC X(10) VALUE 'IN07 08167'.                     
011500        05 FILLER       PIC X(10) VALUE 'IR   05017'.                     
011600        05 FILLER       PIC X(10) VALUE 'IS   01110'.                     
011700        05 FILLER       PIC X(10) VALUE 'IT   01879'.                     
011800        05 FILLER       PIC X(10) VALUE 'IT01 01822'.                     
011900        05 FILLER       PIC X(10) VALUE 'ITL1 01832'.                     
012000        05 FILLER       PIC X(10) VALUE 'JO   05310'.                     
012100        05 FILLER       PIC X(10) VALUE 'JP01 05220'.                     
012200        05 FILLER       PIC X(10) VALUE 'JP09 05222'.                     
012300        05 FILLER       PIC X(10) VALUE 'JPZ4 05220'.                     
012400        05 FILLER       PIC X(10) VALUE 'KE   04115'.                     
012500        05 FILLER       PIC X(10) VALUE 'KH   05216'.                     
012600        05 FILLER       PIC X(10) VALUE 'KR   06124'.                     
012700        05 FILLER       PIC X(10) VALUE 'KR02 06124'.                     
012800        05 FILLER       PIC X(10) VALUE 'KW   05410'.                     
012900        05 FILLER       PIC X(10) VALUE 'KZ   02649'.                     
013000        05 FILLER       PIC X(10) VALUE 'KZ01 02644'.                     
013100        05 FILLER       PIC X(10) VALUE 'LB   05510'.                     
013200        05 FILLER       PIC X(10) VALUE 'LK   06028'.                     
013300        05 FILLER       PIC X(10) VALUE 'LV   02635'.                     
013400        05 FILLER       PIC X(10) VALUE 'LY   04412'.                     
013500        05 FILLER       PIC X(10) VALUE 'LT   02635'.                     
013600        05 FILLER       PIC X(10) VALUE 'MA   03680'.                     
013700        05 FILLER       PIC X(10) VALUE 'MD   02681'.                     
013800        05 FILLER       PIC X(10) VALUE 'MM   05620'.                     
013900        05 FILLER       PIC X(10) VALUE 'MT   03020'.                     
014000        05 FILLER       PIC X(10) VALUE 'MU   04809'.                     
014100        05 FILLER       PIC X(10) VALUE 'MW   03746'.                     
014200        05 FILLER       PIC X(10) VALUE 'MX   08153'.                     
014300        05 FILLER       PIC X(10) VALUE 'MX01 06591'.                     
014400        05 FILLER       PIC X(10) VALUE 'MX09 06591'.                     
014500        05 FILLER       PIC X(10) VALUE 'MX10 06591'.                     
014600        05 FILLER       PIC X(10) VALUE 'MX30 06591'.                     
014700        05 FILLER       PIC X(10) VALUE 'MXZ1 06591'.                     
014800        05 FILLER       PIC X(10) VALUE 'MXZ2 06591'.                     
014900        05 FILLER       PIC X(10) VALUE 'MY   05610'.                     
015000        05 FILLER       PIC X(10) VALUE 'MY04 05619'.                     
015100        05 FILLER       PIC X(10) VALUE 'MZ   03744'.                     
015200        05 FILLER       PIC X(10) VALUE 'NA   04809'.                     
015300        05 FILLER       PIC X(10) VALUE 'NC   07871'.                     
015400        05 FILLER       PIC X(10) VALUE 'NG   03815'.                     
015500        05 FILLER       PIC X(10) VALUE 'NL   01678'.                     
015600        05 FILLER       PIC X(10) VALUE 'NL05 01678'.                     
015700        05 FILLER       PIC X(10) VALUE 'NL10 01678'.                     
015800        05 FILLER       PIC X(10) VALUE 'NLL1 01638'.                     
015900        05 FILLER       PIC X(10) VALUE 'NO   00899'.                     
016000        05 FILLER       PIC X(10) VALUE 'NO01 00878'.                     
016100        05 FILLER       PIC X(10) VALUE 'NO04 00899'.                     
016200        05 FILLER       PIC X(10) VALUE 'NZ   07910'.                     
016300        05 FILLER       PIC X(10) VALUE 'OM   06234'.                     
016400        05 FILLER       PIC X(10) VALUE 'PA   07470'.                     
016500        05 FILLER       PIC X(10) VALUE 'PE   06790'.                     
016600        05 FILLER       PIC X(10) VALUE 'PE02 06790'.                     
016700        05 FILLER       PIC X(10) VALUE 'PE05 06785'.                     
016800        05 FILLER       PIC X(10) VALUE 'PF   07426'.                     
016900        05 FILLER       PIC X(10) VALUE 'PH   06019'.                     
017000        05 FILLER       PIC X(10) VALUE 'PK   06053'.                     
017100        05 FILLER       PIC X(10) VALUE 'PK01 06053'.                     
017200        05 FILLER       PIC X(10) VALUE 'PL01 02870'.                     
017300        05 FILLER       PIC X(10) VALUE 'PL05 02878'.                     
017400        05 FILLER       PIC X(10) VALUE 'PL06 02878'.                     
017500        05 FILLER       PIC X(10) VALUE 'PR   07538'.                     
017600        05 FILLER       PIC X(10) VALUE 'PT   01920'.                     
017700        05 FILLER       PIC X(10) VALUE 'PY   06680'.                     
017800        05 FILLER       PIC X(10) VALUE 'QA   05415'.                     
017900        05 FILLER       PIC X(10) VALUE 'RE   03150'.                     
018000        05 FILLER       PIC X(10) VALUE 'RO   02866'.                     
018100        05 FILLER       PIC X(10) VALUE 'RU   02603'.                     
018200        05 FILLER       PIC X(10) VALUE 'RU03 02671'.                     
018300        05 FILLER       PIC X(10) VALUE 'RUZ1 02602'.                     
018400        05 FILLER       PIC X(10) VALUE 'SA   04840'.                     
018500        05 FILLER       PIC X(10) VALUE 'SE   00069'.                     
018600        05 FILLER       PIC X(10) VALUE 'SE01 00069'.                     
018700        05 FILLER       PIC X(10) VALUE 'SE02 00069'.                     
018800        05 FILLER       PIC X(10) VALUE 'SE03 00778'.                     
018900        05 FILLER       PIC X(10) VALUE 'SE04 00778'.                     
019000        05 FILLER       PIC X(10) VALUE 'SE09 00072'.                     
019100        05 FILLER       PIC X(10) VALUE 'SE12 00072'.                     
019200        05 FILLER       PIC X(10) VALUE 'SE26 00069'.                     
019300        05 FILLER       PIC X(10) VALUE 'SE27 00069'.                     
019400        05 FILLER       PIC X(10) VALUE 'SE28 00069'.                     
019500        05 FILLER       PIC X(10) VALUE 'SE35 00069'.                     
019600        05 FILLER       PIC X(10) VALUE 'SE36 00069'.                     
019700        05 FILLER       PIC X(10) VALUE 'SE38 00069'.                     
019800        05 FILLER       PIC X(10) VALUE 'SE41 00069'.                     
019900        05 FILLER       PIC X(10) VALUE 'SE47 00069'.                     
020000        05 FILLER       PIC X(10) VALUE 'SE51 00072'.                     
020100        05 FILLER       PIC X(10) VALUE 'SE63 00771'.                     
020200        05 FILLER       PIC X(10) VALUE 'SE84 00069'.                     
020300        05 FILLER       PIC X(10) VALUE 'SE91 00069'.                     
020400        05 FILLER       PIC X(10) VALUE 'SEAA 00069'.                     
020500        05 FILLER       PIC X(10) VALUE 'SEAD 00069'.                     
020600        05 FILLER       PIC X(10) VALUE 'SEB1 00069'.                     
020700        05 FILLER       PIC X(10) VALUE 'SEB1 00069'.                     
020800        05 FILLER       PIC X(10) VALUE 'SEC9 00072'.                     
020900        05 FILLER       PIC X(10) VALUE 'SEJ1 00778'.                     
021000        05 FILLER       PIC X(10) VALUE 'SEI3 00738'.                     
021100        05 FILLER       PIC X(10) VALUE 'SEL1 00738'.                     
021200        05 FILLER       PIC X(10) VALUE 'SG   05616'.                     
021300        05 FILLER       PIC X(10) VALUE 'SG01 06208'.                     
021400        05 FILLER       PIC X(10) VALUE 'SI   02450'.                     
021500        05 FILLER       PIC X(10) VALUE 'SK   02386'.                     
021600        05 FILLER       PIC X(10) VALUE 'SR   07080'.                     
021700        05 FILLER       PIC X(10) VALUE 'SU   02605'.                     
021800        05 FILLER       PIC X(10) VALUE 'SV   07590'.                     
021900        05 FILLER       PIC X(10) VALUE 'SY   05725'.                     
022000        05 FILLER       PIC X(10) VALUE 'TH   06225'.                     
022100        05 FILLER       PIC X(10) VALUE 'TH01 06225'.                     
022200        05 FILLER       PIC X(10) VALUE 'TM   02678'.                     
022300        05 FILLER       PIC X(10) VALUE 'TN   04400'.                     
022400        05 FILLER       PIC X(10) VALUE 'TR   05840'.                     
022500        05 FILLER       PIC X(10) VALUE 'TR01 05810'.                     
022600        05 FILLER       PIC X(10) VALUE 'TR02 05810'.                     
022700        05 FILLER       PIC X(10) VALUE 'TT   07436'.                     
022800        05 FILLER       PIC X(10) VALUE 'TW   06221'.                     
022900        05 FILLER       PIC X(10) VALUE 'TW01 06203'.                     
023000        05 FILLER       PIC X(10) VALUE 'TWZ1 06203'.                     
023100        05 FILLER       PIC X(10) VALUE 'UA   02715'.                     
023200        05 FILLER       PIC X(10) VALUE 'UG   04335'.                     
023300        05 FILLER       PIC X(10) VALUE 'UY   06890'.                     
023400        05 FILLER       PIC X(10) VALUE 'US   08617'.                     
023500        05 FILLER       PIC X(10) VALUE 'US01 07574'.                     
023600        05 FILLER       PIC X(10) VALUE 'VN   05210'.                     
023610        05 FILLER       PIC X(10) VALUE 'XK   02445'.                     
023700        05 FILLER       PIC X(10) VALUE 'YE   06231'.                     
023800        05 FILLER       PIC X(10) VALUE 'ZA   03161'.                     
023900        05 FILLER       PIC X(10) VALUE 'ZA04 03160'.                     
024000        05 FILLER       PIC X(10) VALUE 'ZAZ1 03160'.                     
024100        05 FILLER       PIC X(10) VALUE 'ZAZ2 03160'.                     
024200        05 FILLER       PIC X(10) VALUE 'ZM   04327'.                     
024300        05 FILLER       PIC X(10) VALUE 'ZU   03160'.                     
024400        05 FILLER       PIC X(10) VALUE 'ZW   03743'.                     
024500     03 DISTRIKT REDEFINES TRADINGPARTNERS                                
024600                  OCCURS 212                                              
024700                  INDEXED BY IX.                                          
024800        05  KDTRADP-TAB    PIC X(4).                                      
024900        05  FILLER         PIC X.                                         
025000        05  IDDISTR-TAB    PIC 9(5).                                      
025100                                                                          
025200     EJECT                                                                
025300 LINKAGE SECTION.                                                         
025400*01  -COPY W517TRAD                                                       
025500      EJECT                                                               
025600 PROCEDURE DIVISION USING TRAD-W517TRAD.                                  
025700     MOVE '0' TO TRAD-IDDISTR                                             
025800     MOVE '0' TO TRAD-KDSVAR                                              
025900     EVALUATE TRAD-KDCALL                                                 
026000       WHEN 0 PERFORM A-SOK-KDTRADP                                       
026100                                                                          
026200     END-EVALUATE                                                         
026300                                                                          
026400     MOVE ZERO TO RETURN-CODE                                             
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-SOK-KDTRADP SECTION.                                                   
026900     SET IX TO 1                                                          
027000     SEARCH DISTRIKT                                                      
027100       AT END                                                             
027200         MOVE '1'             TO  TRAD-KDSVAR                             
027300       WHEN TRAD-KDTRADP      =   KDTRADP-TAB(IX)                         
027400         MOVE IDDISTR-TAB(IX) TO TRAD-IDDISTR                             
027500     END-SEARCH                                                           
027600     .                                                                    
