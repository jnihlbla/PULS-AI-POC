000100*COMPOPT STDSUB=YES                                                       
000200                                                                          
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.    W510MARK.                                                 
000500                                                                          
000600*    AUTHOR         JAN PETTERSSON.                                       
000700*    DATE-WRITTEN.  OKT 1987.                                             
000800*                                                                         
000900*    REMARKS                                                              
001000*    FUNKTION: ANROP   - "CALL W510MARK USING MARK-W510MARK"              
001100*                                                                         
001200*        BEHANDLING    - DENNA MODUL OMVANDLAR DISTRIKTSNR TILL           
001300*                        KOD FÖR EKONOMIAVDELNINGENS 96-MARKNADS-         
001400*                        GRUPPERING SAMT BENÄMNING FÖR DESSA.             
001500*                      - ELLER HÄMTAR RÄTT BENÄMNING FÖR GIVEN            
001600*                        MARKNADSKOD.                                     
001700*                                                                         
001800*              KDCALL:   0 = OMVANDLA DISTRIKT                            
001900*                        1 = GE 96-MARKNADSTEXTER                         
002000*                                                                         
002100*              KDSVAR:   0 = INGEN FELFÖREKOMST                           
002200*                        1 = DISTRIKT SAKNAS I DISTRIKTSTABELLEN          
002300*                        2 = SÖKKOD SAKNAS I MARKNADSTABELL-96            
002400*                                                                         
002500*             OBS !!!    DISTRIKT SOM EJ ÅTERFINNS I DISTRIKS-            
002600*                        TABELL FÅR MARKNAD 95 MED                        
002700*                        MARKNADSBENÄMNING BLANKT                         
002800*                                                                         
002900*        ÄNDRAD FÖR ETRACKER NO 1500258, INSTALLERAD 2004-10-25           
003000                                                                          
003100 ENVIRONMENT DIVISION.                                                    
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                PIC X(8)    VALUE 'W510MARK'.                   
003800 77  JA                   PIC X       VALUE 'J'.                          
003900 77  NEJ                  PIC X       VALUE 'N'.                          
004000                                                                          
004100**************  OBS !!!!!!!!! OM DU ÄNDRAR TABELLERNA   ********          
004200*************     KOLLA  N O G A  ATT OCCURS ÄR RÄTT      ******          
004300     EJECT                                                                
004400 01  FILLER               PIC X(16)   VALUE 'ALLA DISTRIKT '.             
004500                                                                          
004600 01  TABELL.                                                              
004700     03  DISTRIKTSVARDEN.                                                 
004800         05 FILLER     PIC X(15) VALUE '00001 00001  02'.                 
004900         05 FILLER     PIC X(15) VALUE '00004 00004  02'.                 
005000         05 FILLER     PIC X(15) VALUE '00006 00009  02'.                 
005100         05 FILLER     PIC X(15) VALUE '00015 00015  02'.                 
005200         05 FILLER     PIC X(15) VALUE '00017 00017  02'.                 
005300         05 FILLER     PIC X(15) VALUE '00023 00023  05'.                 
005400         05 FILLER     PIC X(15) VALUE '00025 00025  02'.                 
005500         05 FILLER     PIC X(15) VALUE '00028 00030  02'.                 
005600         05 FILLER     PIC X(15) VALUE '00034 00034  05'.                 
005700         05 FILLER     PIC X(15) VALUE '00035 00035  02'.                 
005800         05 FILLER     PIC X(15) VALUE '00036 00036  01'.                 
005900         05 FILLER     PIC X(15) VALUE '00038 00038  05'.                 
006000         05 FILLER     PIC X(15) VALUE '00041 00041  02'.                 
006100         05 FILLER     PIC X(15) VALUE '00043 00043  02'.                 
006200         05 FILLER     PIC X(15) VALUE '00045 00045  02'.                 
006300         05 FILLER     PIC X(15) VALUE '00048 00048  02'.                 
006400         05 FILLER     PIC X(15) VALUE '00050 00050  02'.                 
006500         05 FILLER     PIC X(15) VALUE '00053 00053  05'.                 
006600         05 FILLER     PIC X(15) VALUE '00054 00054  02'.                 
006700         05 FILLER     PIC X(15) VALUE '00056 00057  02'.                 
006800         05 FILLER     PIC X(15) VALUE '00061 00062  01'.                 
006900         05 FILLER     PIC X(15) VALUE '00066 00067  01'.                 
007000         05 FILLER     PIC X(15) VALUE '00068 00068  02'.                 
007100         05 FILLER     PIC X(15) VALUE '00069 00069  05'.                 
007200         05 FILLER     PIC X(15) VALUE '00072 00072  02'.                 
007300         05 FILLER     PIC X(15) VALUE '00074 00074  02'.                 
007400         05 FILLER     PIC X(15) VALUE '00076 00076  05'.                 
007500         05 FILLER     PIC X(15) VALUE '00077 00077  02'.                 
007600         05 FILLER     PIC X(15) VALUE '00078 00078  05'.                 
007700         05 FILLER     PIC X(15) VALUE '00079 00080  02'.                 
007800         05 FILLER     PIC X(15) VALUE '00082 00083  02'.                 
007900         05 FILLER     PIC X(15) VALUE '00087 00089  02'.                 
008000         05 FILLER     PIC X(15) VALUE '00092 00093  02'.                 
008100         05 FILLER     PIC X(15) VALUE '00096 00099  02'.                 
008200         05 FILLER     PIC X(15) VALUE '00738 00739  01'.                 
008300         05 FILLER     PIC X(15) VALUE '00771 00771  01'.                 
008400         05 FILLER     PIC X(15) VALUE '00778 00778  01'.                 
008500         05 FILLER     PIC X(15) VALUE '00830 00830  10'.                 
008600         05 FILLER     PIC X(15) VALUE '00870 00870  10'.                 
008700         05 FILLER     PIC X(15) VALUE '00878 00878  10'.                 
008800         05 FILLER     PIC X(15) VALUE '00888 00888  12'.                 
008900         05 FILLER     PIC X(15) VALUE '00890 00890  10'.                 
009000         05 FILLER     PIC X(15) VALUE '00899 00899  12'.                 
009100         05 FILLER     PIC X(15) VALUE '00930 00930  07'.                 
009200         05 FILLER     PIC X(15) VALUE '00970 00970  07'.                 
009300         05 FILLER     PIC X(15) VALUE '00974 00974  07'.                 
009400         05 FILLER     PIC X(15) VALUE '00978 00978  07'.                 
009500         05 FILLER     PIC X(15) VALUE '00990 00990  07'.                 
009600         05 FILLER     PIC X(15) VALUE '00999 00999  12'.                 
009700         05 FILLER     PIC X(15) VALUE '01000 01000  08'.                 
009800         05 FILLER     PIC X(15) VALUE '01090 01091  08'.                 
009900         05 FILLER     PIC X(15) VALUE '01099 01099  12'.                 
010000         05 FILLER     PIC X(15) VALUE '01110 01110  09'.                 
010100         05 FILLER     PIC X(15) VALUE '01238 01239  15'.                 
010200         05 FILLER     PIC X(15) VALUE '01241 01241  02'.                 
010300         05 FILLER     PIC X(15) VALUE '01257 01258  15'.                 
010400         05 FILLER     PIC X(15) VALUE '01264 01264  15'.                 
010500         05 FILLER     PIC X(15) VALUE '01271 01271  13'.                 
010600         05 FILLER     PIC X(15) VALUE '01274 01274  15'.                 
010700         05 FILLER     PIC X(15) VALUE '01275 01275  13'.                 
010800         05 FILLER     PIC X(15) VALUE '01276 01276  15'.                 
010900         05 FILLER     PIC X(15) VALUE '01279 01279  33'.                 
011000         05 FILLER     PIC X(15) VALUE '01280 01282  13'.                 
011100         05 FILLER     PIC X(15) VALUE '01283 01283  15'.                 
011200         05 FILLER     PIC X(15) VALUE '01286 01286  13'.                 
011300         05 FILLER     PIC X(15) VALUE '01287 01287  15'.                 
011400         05 FILLER     PIC X(15) VALUE '01291 01292  13'.                 
011500         05 FILLER     PIC X(15) VALUE '01310 01312  30'.                 
011600         05 FILLER     PIC X(15) VALUE '01348 01348  30'.                 
011700         05 FILLER     PIC X(15) VALUE '01360 01360  30'.                 
011800         05 FILLER     PIC X(15) VALUE '01378 01379  30'.                 
011900         05 FILLER     PIC X(15) VALUE '01382 01382  30'.                 
012000         05 FILLER     PIC X(15) VALUE '01420 01420  17'.                 
012100         05 FILLER     PIC X(15) VALUE '01438 01438  17'.                 
012200         05 FILLER     PIC X(15) VALUE '01460 01460  17'.                 
012300         05 FILLER     PIC X(15) VALUE '01470 01470  17'.                 
012400         05 FILLER     PIC X(15) VALUE '01478 01478  17'.                 
012500         05 FILLER     PIC X(15) VALUE '01479 01479  33'.                 
012600         05 FILLER     PIC X(15) VALUE '01515 01516  18'.                 
012700         05 FILLER     PIC X(15) VALUE '05216 05216  95'.                 
012800         05 FILLER     PIC X(15) VALUE '01558 01558  18'.                 
012900         05 FILLER     PIC X(15) VALUE '01578 01579  18'.                 
013000         05 FILLER     PIC X(15) VALUE '01619 01619  20'.                 
013100         05 FILLER     PIC X(15) VALUE '01620 01620  21'.                 
013200         05 FILLER     PIC X(15) VALUE '01621 01624  20'.                 
013300         05 FILLER     PIC X(15) VALUE '01627 01628  20'.                 
013400         05 FILLER     PIC X(15) VALUE '01632 01632  20'.                 
013500         05 FILLER     PIC X(15) VALUE '01634 01634  20'.                 
013600         05 FILLER     PIC X(15) VALUE '01638 01638  21'.                 
013700         05 FILLER     PIC X(15) VALUE '01648 01648  33'.                 
013800         05 FILLER     PIC X(15) VALUE '01650 01650  20'.                 
013900         05 FILLER     PIC X(15) VALUE '01678 01678  21'.                 
014000         05 FILLER     PIC X(15) VALUE '01679 01679  33'.                 
014100         05 FILLER     PIC X(15) VALUE '01730 01730  62'.                 
014200         05 FILLER     PIC X(15) VALUE '01778 01778  62'.                 
014300         05 FILLER     PIC X(15) VALUE '01820 01822  23'.                 
014400         05 FILLER     PIC X(15) VALUE '01832 01832  23'.                 
014500         05 FILLER     PIC X(15) VALUE '01870 01871  23'.                 
014600         05 FILLER     PIC X(15) VALUE '01872 01872  33'.                 
014700         05 FILLER     PIC X(15) VALUE '01879 01879  33'.                 
014800         05 FILLER     PIC X(15) VALUE '01920 01930  26'.                 
014900         05 FILLER     PIC X(15) VALUE '01958 01958  26'.                 
015000         05 FILLER     PIC X(15) VALUE '01978 01978  26'.                 
015100         05 FILLER     PIC X(15) VALUE '01979 01979  33'.                 
015200         05 FILLER     PIC X(15) VALUE '02050 02050  27'.                 
015300         05 FILLER     PIC X(15) VALUE '02070 02070  27'.                 
015400         05 FILLER     PIC X(15) VALUE '02078 02078  27'.                 
015500         05 FILLER     PIC X(15) VALUE '02120 02120  28'.                 
015600         05 FILLER     PIC X(15) VALUE '02138 02138  28'.                 
015700         05 FILLER     PIC X(15) VALUE '02170 02170  28'.                 
015800         05 FILLER     PIC X(15) VALUE '02178 02178  28'.                 
015900         05 FILLER     PIC X(15) VALUE '02179 02179  33'.                 
016000         05 FILLER     PIC X(15) VALUE '02190 02190  28'.                 
016100         05 FILLER     PIC X(15) VALUE '02193 02193  28'.                 
016200         05 FILLER     PIC X(15) VALUE '02196 02196  28'.                 
016300         05 FILLER     PIC X(15) VALUE '02238 02238  31'.                 
016400         05 FILLER     PIC X(15) VALUE '02240 02240  31'.                 
016500         05 FILLER     PIC X(15) VALUE '02259 02259  33'.                 
016600         05 FILLER     PIC X(15) VALUE '02270 02270  31'.                 
016700         05 FILLER     PIC X(15) VALUE '02278 02278  31'.                 
016800         05 FILLER     PIC X(15) VALUE '02282 02282  31'.                 
016900         05 FILLER     PIC X(15) VALUE '02334 02334  32'.                 
017000         05 FILLER     PIC X(15) VALUE '02344 02346  32'.                 
017100         05 FILLER     PIC X(15) VALUE '02359 02359  33'.                 
017200         05 FILLER     PIC X(15) VALUE '02364 02364  37'.                 
017300         05 FILLER     PIC X(15) VALUE '02365 02365  39'.                 
017400         05 FILLER     PIC X(15) VALUE '02370 02373  41'.                 
017500         05 FILLER     PIC X(15) VALUE '02374 02374  37'.                 
017600         05 FILLER     PIC X(15) VALUE '02375 02375  39'.                 
017700         05 FILLER     PIC X(15) VALUE '02376 02376  25'.                 
017800         05 FILLER     PIC X(15) VALUE '02377 02377  41'.                 
017900         05 FILLER     PIC X(15) VALUE '02378 02378  32'.                 
018000         05 FILLER     PIC X(15) VALUE '02380 02380  41'.                 
018100         05 FILLER     PIC X(15) VALUE '02382 02383  41'.                 
018200         05 FILLER     PIC X(15) VALUE '02385 02385  41'.                 
018300         05 FILLER     PIC X(15) VALUE '02386 02386  25'.                 
018400         05 FILLER     PIC X(15) VALUE '02387 02388  41'.                 
018500         05 FILLER     PIC X(15) VALUE '02445 02450  41'.                 
018600         05 FILLER     PIC X(15) VALUE '02459 02459  25'.                 
018700         05 FILLER     PIC X(15) VALUE '02460 02460  41'.                 
018800         05 FILLER     PIC X(15) VALUE '02470 02470  41'.                 
018900         05 FILLER     PIC X(15) VALUE '02480 02480  25'.                 
019000         05 FILLER     PIC X(15) VALUE '02555 02555  37'.                 
019100         05 FILLER     PIC X(15) VALUE '02560 02561  34'.                 
019200         05 FILLER     PIC X(15) VALUE '02579 02579  34'.                 
019300         05 FILLER     PIC X(15) VALUE '02600 02601  38'.                 
019400         05 FILLER     PIC X(15) VALUE '02602 02603  38'.                 
019500         05 FILLER     PIC X(15) VALUE '02604 02604  24'.                 
019600         05 FILLER     PIC X(15) VALUE '02605 02611  38'.                 
019700         05 FILLER     PIC X(15) VALUE '02612 02612  42'.                 
019800         05 FILLER     PIC X(15) VALUE '02613 02614  38'.                 
019900         05 FILLER     PIC X(15) VALUE '02615 02617  63'.                 
020000         05 FILLER     PIC X(15) VALUE '02619 02619  29'.                 
020100         05 FILLER     PIC X(15) VALUE '02620 02620  38'.                 
020200         05 FILLER     PIC X(15) VALUE '02621 02621  69'.                 
020300         05 FILLER     PIC X(15) VALUE '02622 02622  38'.                 
020400         05 FILLER     PIC X(15) VALUE '02623 02623  69'.                 
020500         05 FILLER     PIC X(15) VALUE '02624 02625  38'.                 
020600         05 FILLER     PIC X(15) VALUE '02626 02626  47'.                 
020700         05 FILLER     PIC X(15) VALUE '02627 02628  38'.                 
020800         05 FILLER     PIC X(15) VALUE '02629 02629  63'.                 
020900         05 FILLER     PIC X(15) VALUE '02630 02630  42'.                 
021000         05 FILLER     PIC X(15) VALUE '02632 02633  29'.                 
021100         05 FILLER     PIC X(15) VALUE '02634 02634  47'.                 
021200         05 FILLER     PIC X(15) VALUE '02635 02635  42'.                 
021300         05 FILLER     PIC X(15) VALUE '02636 02642  38'.                 
021400         05 FILLER     PIC X(15) VALUE '02643 02643  47'.                 
021500         05 FILLER     PIC X(15) VALUE '02644 02644  40'.                 
021600         05 FILLER     PIC X(15) VALUE '02645 02646  24'.                 
021700         05 FILLER     PIC X(15) VALUE '02647 02648  38'.                 
021800         05 FILLER     PIC X(15) VALUE '02649 02649  40'.                 
021900         05 FILLER     PIC X(15) VALUE '02650 02651  29'.                 
022000         05 FILLER     PIC X(15) VALUE '02652 02652  29'.                 
022100         05 FILLER     PIC X(15) VALUE '02653 02653  42'.                 
022200         05 FILLER     PIC X(15) VALUE '02655 02655  40'.                 
022300         05 FILLER     PIC X(15) VALUE '02656 02656  24'.                 
022400         05 FILLER     PIC X(15) VALUE '02657 02658  38'.                 
022500         05 FILLER     PIC X(15) VALUE '02660 02660  38'.                 
022600         05 FILLER     PIC X(15) VALUE '02661 02662  63'.                 
022700         05 FILLER     PIC X(15) VALUE '02663 02664  38'.                 
022800         05 FILLER     PIC X(15) VALUE '02665 02665  57'.                 
022900         05 FILLER     PIC X(15) VALUE '02666 02672  38'.                 
023000         05 FILLER     PIC X(15) VALUE '02673 02673  06'.                 
023100         05 FILLER     PIC X(15) VALUE '02674 02674  29'.                 
023200         05 FILLER     PIC X(15) VALUE '02675 02676  38'.                 
023300         05 FILLER     PIC X(15) VALUE '02677 02677  06'.                 
023400         05 FILLER     PIC X(15) VALUE '02678 02678  57'.                 
023500         05 FILLER     PIC X(15) VALUE '02679 02680  42'.                 
023600         05 FILLER     PIC X(15) VALUE '02681 02681  69'.                 
023700         05 FILLER     PIC X(15) VALUE '02682 02687  38'.                 
023800         05 FILLER     PIC X(15) VALUE '02688 02688  06'.                 
023900         05 FILLER     PIC X(15) VALUE '02696 02696  57'.                 
024000         05 FILLER     PIC X(15) VALUE '02697 02699  38'.                 
024100         05 FILLER     PIC X(15) VALUE '02751 02751  39'.                 
024200         05 FILLER     PIC X(15) VALUE '02843 02843  35'.                 
024300         05 FILLER     PIC X(15) VALUE '02849 02849  35'.                 
024400         05 FILLER     PIC X(15) VALUE '02865 02867  36'.                 
024500         05 FILLER     PIC X(15) VALUE '02870 02870  35'.                 
024600         05 FILLER     PIC X(15) VALUE '02878 02878  35'.                 
024700         05 FILLER     PIC X(15) VALUE '03001 03001  05'.                 
024800         05 FILLER     PIC X(15) VALUE '03020 03020  44'.                 
024900         05 FILLER     PIC X(15) VALUE '03130 03130  04'.                 
025000         05 FILLER     PIC X(15) VALUE '03150 03150  69'.                 
025100         05 FILLER     PIC X(15) VALUE '03160 03162  48'.                 
025200         05 FILLER     PIC X(15) VALUE '03163 03164  48'.                 
025300         05 FILLER     PIC X(15) VALUE '03166 03167  48'.                 
025400         05 FILLER     PIC X(15) VALUE '03235 03235  49'.                 
025500         05 FILLER     PIC X(15) VALUE '03250 03250  49'.                 
025600         05 FILLER     PIC X(15) VALUE '03403 03403  58'.                 
025700         05 FILLER     PIC X(15) VALUE '03439 03439  69'.                 
025800         05 FILLER     PIC X(15) VALUE '03471 03471  69'.                 
025900         05 FILLER     PIC X(15) VALUE '03476 03476  69'.                 
026000         05 FILLER     PIC X(15) VALUE '03660 03660  53'.                 
026100         05 FILLER     PIC X(15) VALUE '03670 03670  53'.                 
026200         05 FILLER     PIC X(15) VALUE '03680 03680  53'.                 
026300         05 FILLER     PIC X(15) VALUE '03740 03740  48'.                 
026400         05 FILLER     PIC X(15) VALUE '03743 03746  58'.                 
026500         05 FILLER     PIC X(15) VALUE '03812 03812  55'.                 
026600         05 FILLER     PIC X(15) VALUE '03814 03814  55'.                 
026700         05 FILLER     PIC X(15) VALUE '03815 03815  55'.                 
026800         05 FILLER     PIC X(15) VALUE '03817 03817  55'.                 
026900         05 FILLER     PIC X(15) VALUE '04015 04015  48'.                 
027000         05 FILLER     PIC X(15) VALUE '04115 04115  51'.                 
027100         05 FILLER     PIC X(15) VALUE '04120 04120  51'.                 
027200         05 FILLER     PIC X(15) VALUE '04320 04320  58'.                 
027300         05 FILLER     PIC X(15) VALUE '04325 04327  58'.                 
027400         05 FILLER     PIC X(15) VALUE '04330 04330  69'.                 
027500         05 FILLER     PIC X(15) VALUE '04335 04335  69'.                 
027600         05 FILLER     PIC X(15) VALUE '04369 04369  57'.                 
027700         05 FILLER     PIC X(15) VALUE '04400 04401  58'.                 
027800         05 FILLER     PIC X(15) VALUE '04410 04410  95'.                 
027900         05 FILLER     PIC X(15) VALUE '04412 04412  52'.                 
028000         05 FILLER     PIC X(15) VALUE '04470 04470  69'.                 
028100         05 FILLER     PIC X(15) VALUE '04550 04550  58'.                 
028200         05 FILLER     PIC X(15) VALUE '04560 04570  58'.                 
028300         05 FILLER     PIC X(15) VALUE '04610 04612  58'.                 
028400         05 FILLER     PIC X(15) VALUE '04622 04622  69'.                 
028500         05 FILLER     PIC X(15) VALUE '04809 04809  58'.                 
028600         05 FILLER     PIC X(15) VALUE '04810 04810  95'.                 
028700         05 FILLER     PIC X(15) VALUE '04811 04811  69'.                 
028800         05 FILLER     PIC X(15) VALUE '04833 04833  58'.                 
028900         05 FILLER     PIC X(15) VALUE '04834 04836  52'.                 
029000         05 FILLER     PIC X(15) VALUE '04839 04839  69'.                 
029100         05 FILLER     PIC X(15) VALUE '04840 04848  66'.                 
029200         05 FILLER     PIC X(15) VALUE '04849 04849  69'.                 
029300         05 FILLER     PIC X(15) VALUE '04850 04850  66'.                 
029400         05 FILLER     PIC X(15) VALUE '04852 04852  57'.                 
029500         05 FILLER     PIC X(15) VALUE '04859 04859  69'.                 
029600         05 FILLER     PIC X(15) VALUE '04860 04860  66'.                 
029700         05 FILLER     PIC X(15) VALUE '05017 05017  63'.                 
029800         05 FILLER     PIC X(15) VALUE '05039 05039  63'.                 
029900         05 FILLER     PIC X(15) VALUE '05120 05121  22'.                 
030000         05 FILLER     PIC X(15) VALUE '05210 05212  16'.                 
030100         05 FILLER     PIC X(15) VALUE '05214 05214  16'.                 
030200         05 FILLER     PIC X(15) VALUE '05220 05220  81'.                 
030300         05 FILLER     PIC X(15) VALUE '05222 05223  81'.                 
030400         05 FILLER     PIC X(15) VALUE '05225 05225  81'.                 
030500         05 FILLER     PIC X(15) VALUE '05230 05230  81'.                 
030600         05 FILLER     PIC X(15) VALUE '05310 05314  45'.                 
030700         05 FILLER     PIC X(15) VALUE '05400 05400  65'.                 
030800         05 FILLER     PIC X(15) VALUE '05410 05410  65'.                 
030900         05 FILLER     PIC X(15) VALUE '05415 05415  64'.                 
031000         05 FILLER     PIC X(15) VALUE '05420 05420  69'.                 
031100         05 FILLER     PIC X(15) VALUE '05510 05510  54'.                 
031200         05 FILLER     PIC X(15) VALUE '05610 05612  58'.                 
031300         05 FILLER     PIC X(15) VALUE '05616 05616  88'.                 
031400         05 FILLER     PIC X(15) VALUE '05617 05619  85'.                 
031500         05 FILLER     PIC X(15) VALUE '05620 05620  16'.                 
031600         05 FILLER     PIC X(15) VALUE '05624 05625  86'.                 
031700         05 FILLER     PIC X(15) VALUE '05627 05627  85'.                 
031800         05 FILLER     PIC X(15) VALUE '05639 05639  85'.                 
031900         05 FILLER     PIC X(15) VALUE '05670 05670  85'.                 
032000         05 FILLER     PIC X(15) VALUE '05725 05725  67'.                 
032100         05 FILLER     PIC X(15) VALUE '05735 05735  67'.                 
032200         05 FILLER     PIC X(15) VALUE '05739 05739  67'.                 
032300         05 FILLER     PIC X(15) VALUE '05810 05812  68'.                 
032400         05 FILLER     PIC X(15) VALUE '05819 05820  68'.                 
032500         05 FILLER     PIC X(15) VALUE '05826 05827  68'.                 
032600         05 FILLER     PIC X(15) VALUE '05840 05840  57'.                 
032700         05 FILLER     PIC X(15) VALUE '05913 05913  80'.                 
032800         05 FILLER     PIC X(15) VALUE '05916 05918  80'.                 
032900         05 FILLER     PIC X(15) VALUE '05920 05921  80'.                 
033000         05 FILLER     PIC X(15) VALUE '05922 05922  74'.                 
033100         05 FILLER     PIC X(15) VALUE '06010 06010  75'.                 
033200         05 FILLER     PIC X(15) VALUE '06016 06017  93'.                 
033300         05 FILLER     PIC X(15) VALUE '06019 06019  93'.                 
033400         05 FILLER     PIC X(15) VALUE '06020 06021  43'.                 
033500         05 FILLER     PIC X(15) VALUE '06025 06026  69'.                 
033600         05 FILLER     PIC X(15) VALUE '06027 06027  75'.                 
033700         05 FILLER     PIC X(15) VALUE '06028 06028  11'.                 
033800         05 FILLER     PIC X(15) VALUE '06029 06029  75'.                 
033900         05 FILLER     PIC X(15) VALUE '06030 06031  75'.                 
034000         05 FILLER     PIC X(15) VALUE '06032 06033  75'.                 
034100         05 FILLER     PIC X(15) VALUE '06034 06034  75'.                 
034200         05 FILLER     PIC X(15) VALUE '06035 06035  11'.                 
034300         05 FILLER     PIC X(15) VALUE '06036 06039  75'.                 
034400         05 FILLER     PIC X(15) VALUE '06040 06040  70'.                 
034500         05 FILLER     PIC X(15) VALUE '06041 06049  75'.                 
034600         05 FILLER     PIC X(15) VALUE '06050 06052  75'.                 
034700         05 FILLER     PIC X(15) VALUE '06053 06053  16'.                 
034800         05 FILLER     PIC X(15) VALUE '06054 06059  75'.                 
034900         05 FILLER     PIC X(15) VALUE '06080 06080  75'.                 
035000         05 FILLER     PIC X(15) VALUE '06117 06117  92'.                 
035100         05 FILLER     PIC X(15) VALUE '06121 06122  92'.                 
035200         05 FILLER     PIC X(15) VALUE '06124 06124  92'.                 
035300         05 FILLER     PIC X(15) VALUE '06180 06180  92'.                 
035400         05 FILLER     PIC X(15) VALUE '06200 06200  90'.                 
035500         05 FILLER     PIC X(15) VALUE '06201 06202  83'.                 
035600         05 FILLER     PIC X(15) VALUE '06203 06203  90'.                 
035700         05 FILLER     PIC X(15) VALUE '06204 06204  88'.                 
035800         05 FILLER     PIC X(15) VALUE '06205 06205  83'.                 
035900         05 FILLER     PIC X(15) VALUE '06206 06206  16'.                 
036000         05 FILLER     PIC X(15) VALUE '06207 06207  90'.                 
036100         05 FILLER     PIC X(15) VALUE '06208 06209  16'.                 
036200         05 FILLER     PIC X(15) VALUE '06210 06210  50'.                 
036300         05 FILLER     PIC X(15) VALUE '06211 06211  79'.                 
036400         05 FILLER     PIC X(15) VALUE '06212 06212  83'.                 
036500         05 FILLER     PIC X(15) VALUE '06214 06214  83'.                 
036600         05 FILLER     PIC X(15) VALUE '06215 06216  79'.                 
036700         05 FILLER     PIC X(15) VALUE '06217 06218  83'.                 
036800         05 FILLER     PIC X(15) VALUE '06220 06220  83'.                 
036900         05 FILLER     PIC X(15) VALUE '06221 06224  90'.                 
037000         05 FILLER     PIC X(15) VALUE '06225 06225  91'.                 
037100         05 FILLER     PIC X(15) VALUE '06226 06227  83'.                 
037200         05 FILLER     PIC X(15) VALUE '06228 06229  91'.                 
037300         05 FILLER     PIC X(15) VALUE '06230 06231  56'.                 
037400         05 FILLER     PIC X(15) VALUE '06232 06232  83'.                 
037500         05 FILLER     PIC X(15) VALUE '06233 06234  61'.                 
037600         05 FILLER     PIC X(15) VALUE '06236 06236  60'.                 
037700         05 FILLER     PIC X(15) VALUE '06238 06238  83'.                 
037800         05 FILLER     PIC X(15) VALUE '06240 06240  79'.                 
037900         05 FILLER     PIC X(15) VALUE '06244 06244  69'.                 
038000         05 FILLER     PIC X(15) VALUE '06245 06245  91'.                 
038100         05 FILLER     PIC X(15) VALUE '06246 06246  60'.                 
038200         05 FILLER     PIC X(15) VALUE '06247 06247  59'.                 
038300         05 FILLER     PIC X(15) VALUE '06248 06248  64'.                 
038400         05 FILLER     PIC X(15) VALUE '06250 06250  83'.                 
038500         05 FILLER     PIC X(15) VALUE '06251 06251  91'.                 
038600         05 FILLER     PIC X(15) VALUE '06253 06253  83'.                 
038700         05 FILLER     PIC X(15) VALUE '06258 06258  83'.                 
038800         05 FILLER     PIC X(15) VALUE '06260 06260  83'.                 
038900         05 FILLER     PIC X(15) VALUE '06263 06263  79'.                 
039000         05 FILLER     PIC X(15) VALUE '06264 06264  91'.                 
039100         05 FILLER     PIC X(15) VALUE '06270 06272  83'.                 
039200         05 FILLER     PIC X(15) VALUE '06274 06274  83'.                 
039300         05 FILLER     PIC X(15) VALUE '06280 06280  83'.                 
039310         05 FILLER     PIC X(15) VALUE '06282 06282  91'.                 
039400         05 FILLER     PIC X(15) VALUE '06310 06320  82'.                 
039500         05 FILLER     PIC X(15) VALUE '06390 06392  82'.                 
039600         05 FILLER     PIC X(15) VALUE '06394 06396  82'.                 
039700         05 FILLER     PIC X(15) VALUE '06480 06480  72'.                 
039800         05 FILLER     PIC X(15) VALUE '06486 06486  68'.                 
039900         05 FILLER     PIC X(15) VALUE '06561 06561  84'.                 
040000         05 FILLER     PIC X(15) VALUE '06580 06580  84'.                 
040100         05 FILLER     PIC X(15) VALUE '06583 06583  84'.                 
040200         05 FILLER     PIC X(15) VALUE '06585 06585  84'.                 
040300         05 FILLER     PIC X(15) VALUE '06587 06587  84'.                 
040400         05 FILLER     PIC X(15) VALUE '06588 06589  84'.                 
040500         05 FILLER     PIC X(15) VALUE '06590 06591  84'.                 
040600         05 FILLER     PIC X(15) VALUE '06680 06680  89'.                 
040700         05 FILLER     PIC X(15) VALUE '06780 06780  76'.                 
040800         05 FILLER     PIC X(15) VALUE '06785 06786  76'.                 
040900         05 FILLER     PIC X(15) VALUE '06790 06790  76'.                 
041000         05 FILLER     PIC X(15) VALUE '06801 06801  03'.                 
041100         05 FILLER     PIC X(15) VALUE '06890 06890  03'.                 
041200         05 FILLER     PIC X(15) VALUE '06895 06895  03'.                 
041300         05 FILLER     PIC X(15) VALUE '06898 06898  03'.                 
041400         05 FILLER     PIC X(15) VALUE '06990 06990  78'.                 
041500         05 FILLER     PIC X(15) VALUE '07010 07010  71'.                 
041600         05 FILLER     PIC X(15) VALUE '07020 07026  71'.                 
041700         05 FILLER     PIC X(15) VALUE '07030 07030  71'.                 
041800         05 FILLER     PIC X(15) VALUE '07039 07051  71'.                 
041900         05 FILLER     PIC X(15) VALUE '07060 07060  78'.                 
042000         05 FILLER     PIC X(15) VALUE '07070 07070  71'.                 
042100         05 FILLER     PIC X(15) VALUE '07080 07080  78'.                 
042200         05 FILLER     PIC X(15) VALUE '07170 07170  71'.                 
042300         05 FILLER     PIC X(15) VALUE '07180 07180  78'.                 
042400         05 FILLER     PIC X(15) VALUE '07190 07190  95'.                 
042500         05 FILLER     PIC X(15) VALUE '07290 07290  95'.                 
042600         05 FILLER     PIC X(15) VALUE '07390 07390  95'.                 
042700         05 FILLER     PIC X(15) VALUE '07399 07399  78'.                 
042800         05 FILLER     PIC X(15) VALUE '07411 07412  19'.                 
042900         05 FILLER     PIC X(15) VALUE '07415 07415  19'.                 
043000         05 FILLER     PIC X(15) VALUE '07416 07416  78'.                 
043100         05 FILLER     PIC X(15) VALUE '07425 07426  16'.                 
043200         05 FILLER     PIC X(15) VALUE '07434 07434  78'.                 
043300         05 FILLER     PIC X(15) VALUE '07436 07436  78'.                 
043400         05 FILLER     PIC X(15) VALUE '07440 07440  78'.                 
043500         05 FILLER     PIC X(15) VALUE '07450 07450  78'.                 
043600         05 FILLER     PIC X(15) VALUE '07470 07470  78'.                 
043700         05 FILLER     PIC X(15) VALUE '07472 07472  78'.                 
043800         05 FILLER     PIC X(15) VALUE '07473 07474  77'.                 
044000         05 FILLER     PIC X(15) VALUE '07481 07482  73'.                 
044100         05 FILLER     PIC X(15) VALUE '07484 07484  14'.                 
044200         05 FILLER     PIC X(15) VALUE '07485 07486  78'.                 
044300         05 FILLER     PIC X(15) VALUE '07488 07488  14'.                 
044400         05 FILLER     PIC X(15) VALUE '07489 07489  95'.                 
044500         05 FILLER     PIC X(15) VALUE '07490 07490  14'.                 
044600         05 FILLER     PIC X(15) VALUE '07491 07491  78'.                 
044700         05 FILLER     PIC X(15) VALUE '07495 07496  78'.                 
044800         05 FILLER     PIC X(15) VALUE '07497 07497  14'.                 
044900         05 FILLER     PIC X(15) VALUE '07498 07498  78'.                 
045000         05 FILLER     PIC X(15) VALUE '07510 07510  43'.                 
045100         05 FILLER     PIC X(15) VALUE '07512 07512  43'.                 
045200         05 FILLER     PIC X(15) VALUE '07515 07515  43'.                 
045300         05 FILLER     PIC X(15) VALUE '07531 07531  43'.                 
045400         05 FILLER     PIC X(15) VALUE '07538 07538  77'.                 
045500         05 FILLER     PIC X(15) VALUE '07552 07552  43'.                 
045600         05 FILLER     PIC X(15) VALUE '07573 07575  43'.                 
045700         05 FILLER     PIC X(15) VALUE '07580 07580  43'.                 
045800         05 FILLER     PIC X(15) VALUE '07590 07590  95'.                 
045900         05 FILLER     PIC X(15) VALUE '07620 07620  46'.                 
046000         05 FILLER     PIC X(15) VALUE '07625 07625  46'.                 
046010         05 FILLER     PIC X(15) VALUE '07656 07656  78'.                 
046100         05 FILLER     PIC X(15) VALUE '07674 07674  46'.                 
046200         05 FILLER     PIC X(15) VALUE '07680 07680  46'.                 
046300         05 FILLER     PIC X(15) VALUE '07690 07690  95'.                 
046400         05 FILLER     PIC X(15) VALUE '07830 07830  94'.                 
046500         05 FILLER     PIC X(15) VALUE '07834 07836  94'.                 
046600         05 FILLER     PIC X(15) VALUE '07838 07838  94'.                 
046700         05 FILLER     PIC X(15) VALUE '07870 07871  16'.                 
046800         05 FILLER     PIC X(15) VALUE '07899 07899  03'.                 
046900         05 FILLER     PIC X(15) VALUE '07910 07911  74'.                 
047000         05 FILLER     PIC X(15) VALUE '07920 07920  74'.                 
047100         05 FILLER     PIC X(15) VALUE '07925 07925  74'.                 
047200         05 FILLER     PIC X(15) VALUE '08111 08111  43'.                 
047300         05 FILLER     PIC X(15) VALUE '08141 08147  43'.                 
047400         05 FILLER     PIC X(15) VALUE '08151 08151  46'.                 
047500         05 FILLER     PIC X(15) VALUE '08161 08161  81'.                 
047600         05 FILLER     PIC X(15) VALUE '08162 08162  94'.                 
047700         05 FILLER     PIC X(15) VALUE '08163 08163  91'.                 
047710         05 FILLER     PIC X(15) VALUE '08164 08164  90'.                 
047800         05 FILLER     PIC X(15) VALUE '08165 08165  92'.                 
047900         05 FILLER     PIC X(15) VALUE '08166 08166  85'.                 
048000         05 FILLER     PIC X(15) VALUE '08167 08167  75'.                 
048100         05 FILLER     PIC X(15) VALUE '08171 08174  83'.                 
048200         05 FILLER     PIC X(15) VALUE '08186 08186  68'.                 
048300         05 FILLER     PIC X(15) VALUE '08187 08187  59'.                 
048400         05 FILLER     PIC X(15) VALUE '08200 08201  83'.                 
048500         05 FILLER     PIC X(15) VALUE '08202 08202  75'.                 
048510         05 FILLER     PIC X(15) VALUE '08204 08205  92'.                 
048600         05 FILLER     PIC X(15) VALUE '08211 08211  43'.                 
048700         05 FILLER     PIC X(15) VALUE '08223 08223  68'.                 
048800         05 FILLER     PIC X(15) VALUE '08225 08225  59'.                 
048900         05 FILLER     PIC X(15) VALUE '08490 08490  43'.                 
049000         05 FILLER     PIC X(15) VALUE '08541 08543  43'.                 
049100         05 FILLER     PIC X(15) VALUE '08551 08551  46'.                 
049200         05 FILLER     PIC X(15) VALUE '08614 08614  43'.                 
049300         05 FILLER     PIC X(15) VALUE '08617 08617  43'.                 
049400         05 FILLER     PIC X(15) VALUE '08679 08679  16'.                 
049500         05 FILLER     PIC X(15) VALUE '09141 09141  43'.                 
049600         05 FILLER     PIC X(15) VALUE '09143 09147  43'.                 
049610         05 FILLER     PIC X(15) VALUE '09153 09153  84'.                 
049620         05 FILLER     PIC X(15) VALUE '09165 09165  92'.                 
049700     03  DISTRIKT REDEFINES DISTRIKTSVARDEN                               
049800                       OCCURS 454                                         
049900             INDEXED BY IX.                                               
050000         05  DISTR-FROM         PIC 9(5).                                 
050100         05  FILLER             PIC X(1).                                 
050200         05  DISTR-TOM          PIC 9(5).                                 
050300         05  FILLER             PIC X(2).                                 
050400         05  MARKNAD-96         PIC 9(2).                                 
050500     EJECT                                                                
050600 01  FILLER                  PIC X(16)   VALUE 'MARKNADER '.              
050700                                                                          
050800 01  TABELL-96.                                                           
050900     03  TAB-96-TEXTER.                                                   
051000        05 MARK01      PIC X(24) VALUE 'SWEDEN VCC          '.            
051100        05 MARK02      PIC X(24) VALUE 'SWEDEN PROD         '.            
051200        05 MARK03      PIC X(24) VALUE 'URUGUAY             '.            
051300        05 MARK04      PIC X(24) VALUE 'ANGOLA              '.            
051400        05 MARK05      PIC X(24) VALUE 'MISC SWEDEN         '.            
051500        05 MARK06      PIC X(24) VALUE 'GEORGIA             '.            
051600        05 MARK07      PIC X(24) VALUE 'DENMARK             '.            
051700        05 MARK08      PIC X(24) VALUE 'FINLAND             '.            
051800        05 MARK09      PIC X(24) VALUE 'ICELAND             '.            
051900        05 MARK10      PIC X(24) VALUE 'NORWAY              '.            
052000        05 MARK11      PIC X(24) VALUE 'SRI LANKA           '.            
052100        05 MARK12      PIC X(24) VALUE 'MISC NORDIC         '.            
052200        05 MARK13      PIC X(24) VALUE 'BELGIUM PROD        '.            
052300        05 MARK14      PIC X(24) VALUE 'GUATEMALA           '.            
052400        05 MARK15      PIC X(24) VALUE 'BELGIUM             '.            
052500        05 MARK16      PIC X(24) VALUE 'MISC VCAP           '.            
052600        05 MARK17      PIC X(24) VALUE 'FRANCE              '.            
052700        05 MARK18      PIC X(24) VALUE 'GREECE              '.            
052800        05 MARK19      PIC X(24) VALUE 'BOCA/JAMAICA        '.            
052900        05 MARK20      PIC X(24) VALUE 'HOLLAND PROD        '.            
053000        05 MARK21      PIC X(24) VALUE 'HOLLAND             '.            
053100        05 MARK22      PIC X(24) VALUE 'ISRAEL              '.            
053200        05 MARK23      PIC X(24) VALUE 'ITALY               '.            
053300        05 MARK24      PIC X(24) VALUE 'LATVIA              '.            
053400        05 MARK25      PIC X(24) VALUE 'SLOVAKIEN           '.            
053500        05 MARK26      PIC X(24) VALUE 'PORTUGAL            '.            
053600        05 MARK27      PIC X(24) VALUE 'SWITZERLAND         '.            
053700        05 MARK28      PIC X(24) VALUE 'SPAIN               '.            
053800        05 MARK29      PIC X(24) VALUE 'LITHUANIA           '.            
053900        05 MARK30      PIC X(24) VALUE 'GREAT BRITAIN       '.            
054000        05 MARK31      PIC X(24) VALUE 'GERMANY             '.            
054100        05 MARK32      PIC X(24) VALUE 'AUSTRIA             '.            
054200        05 MARK33      PIC X(24) VALUE 'MISC PROD           '.            
054300        05 MARK34      PIC X(24) VALUE 'BULGARIA            '.            
054400        05 MARK35      PIC X(24) VALUE 'POLAND              '.            
054500        05 MARK36      PIC X(24) VALUE 'ROMANIA             '.            
054600        05 MARK37      PIC X(24) VALUE 'HUNGARY             '.            
054700        05 MARK38      PIC X(24) VALUE 'RUSSIA              '.            
054800        05 MARK39      PIC X(24) VALUE 'CZECH REPUBLIC      '.            
054900        05 MARK40      PIC X(24) VALUE 'KAZAKHSTAN          '.            
055000        05 MARK41      PIC X(24) VALUE 'SLOVENIA + SOME     '.            
055100        05 MARK42      PIC X(24) VALUE 'ESTONIA             '.            
055200        05 MARK43      PIC X(24) VALUE 'USA                 '.            
055300        05 MARK44      PIC X(24) VALUE 'MALTA               '.            
055400        05 MARK45      PIC X(24) VALUE 'JORDANIEN           '.            
055500        05 MARK46      PIC X(24) VALUE 'CANADA              '.            
055600        05 MARK47      PIC X(24) VALUE 'BELARUS             '.            
055700        05 MARK48      PIC X(24) VALUE 'SOUTH AFRICA        '.            
055800        05 MARK49      PIC X(24) VALUE 'EGYPT               '.            
055900        05 MARK50      PIC X(24) VALUE 'CYPRUS              '.            
056000        05 MARK51      PIC X(24) VALUE 'KENYA               '.            
056100        05 MARK52      PIC X(24) VALUE 'LIBYA               '.            
056200        05 MARK53      PIC X(24) VALUE 'MOROCCO             '.            
056300        05 MARK54      PIC X(24) VALUE 'LEBANON             '.            
056400        05 MARK55      PIC X(24) VALUE 'NIGERIA             '.            
056500        05 MARK56      PIC X(24) VALUE 'YEMEN               '.            
056600        05 MARK57      PIC X(24) VALUE 'MISC ISTANBUL HUB   '.            
056700        05 MARK58      PIC X(24) VALUE 'MISC PRETORIA HUB   '.            
056800        05 MARK59      PIC X(24) VALUE 'UAE                 '.            
056900        05 MARK60      PIC X(24) VALUE 'BAHARAIN            '.            
057000        05 MARK61      PIC X(24) VALUE 'OMAN                '.            
057100        05 MARK62      PIC X(24) VALUE 'IRELAND             '.            
057200        05 MARK63      PIC X(24) VALUE 'UKRAINE             '.            
057300        05 MARK64      PIC X(24) VALUE 'QATAR               '.            
057400        05 MARK65      PIC X(24) VALUE 'KUWAIT              '.            
057500        05 MARK66      PIC X(24) VALUE 'SAUDI ARABIA        '.            
057600        05 MARK67      PIC X(24) VALUE 'SYRIA               '.            
057700        05 MARK68      PIC X(24) VALUE 'TURKEY              '.            
057800        05 MARK69      PIC X(24) VALUE 'MISC GOTHENBURG HUB '.            
057900        05 MARK70      PIC X(24) VALUE 'MISC DUBAI HUB      '.            
058000        05 MARK71      PIC X(24) VALUE 'BRAZIL              '.            
058100        05 MARK72      PIC X(24) VALUE 'CHILE               '.            
058200        05 MARK73      PIC X(24) VALUE 'COLOMBIA            '.            
058300        05 MARK74      PIC X(24) VALUE 'NEW ZEALAND + FIDJI '.            
058400        05 MARK75      PIC X(24) VALUE 'INDIEN              '.            
058500        05 MARK76      PIC X(24) VALUE 'PERU                '.            
058600        05 MARK77      PIC X(24) VALUE 'PUERTO RICO         '.            
058700        05 MARK78      PIC X(24) VALUE 'MISC SAO PAULO HUB  '.            
058800        05 MARK79      PIC X(24) VALUE 'HONG KONG           '.            
058900        05 MARK80      PIC X(24) VALUE 'INDONESIA           '.            
059000        05 MARK81      PIC X(24) VALUE 'JAPAN               '.            
059100        05 MARK82      PIC X(24) VALUE 'ARGENTINA           '.            
059200        05 MARK83      PIC X(24) VALUE 'CHINA               '.            
059300        05 MARK84      PIC X(24) VALUE 'MEXICO              '.            
059400        05 MARK85      PIC X(24) VALUE 'MALAYSIA            '.            
059500        05 MARK86      PIC X(24) VALUE 'BRUNEI              '.            
059600        05 MARK87      PIC X(24) VALUE 'BANGLADESH          '.            
059700        05 MARK88      PIC X(24) VALUE 'SINGAPORE           '.            
059800        05 MARK89      PIC X(24) VALUE 'PARAGUAY            '.            
059900        05 MARK90      PIC X(24) VALUE 'TAIWAN              '.            
060000        05 MARK91      PIC X(24) VALUE 'THAILAND            '.            
060100        05 MARK92      PIC X(24) VALUE 'SOUTH KOREA         '.            
060200        05 MARK93      PIC X(24) VALUE 'PHILIPPINES         '.            
060300        05 MARK94      PIC X(24) VALUE 'AUSTRALIA           '.            
060400        05 MARK95      PIC X(24) VALUE 'MISC OTHERS         '.            
060500        05 MARK96      PIC X(24) VALUE 'EJ FÖRD.BART EX     '.            
060600     03  MARKNADER-96 REDEFINES TAB-96-TEXTER                             
060700                       OCCURS 96.                                         
060800         05  BEMARKNAD-96 PIC X(24).                                      
060900     EJECT                                                                
061000 LINKAGE SECTION.                                                         
061100                                                                          
061200*01  -COPY W510MARK                                                       
061300     EJECT                                                                
061400 PROCEDURE DIVISION USING MARK-W510MARK.                                  
061500                                                                          
061600     MOVE SPACE                         TO MARK-BEMARKN                   
061700     MOVE '0'                           TO MARK-KDSVAR                    
061800                                                                          
061900     EVALUATE MARK-KDCALL                                                 
062000                                                                          
062100           WHEN 0  PERFORM A-SOK-DISTR-GE-MARKNAD                         
062200                                                                          
062300           WHEN 1  PERFORM B-SOK-MARKN96-GE-BEN                           
062400                                                                          
062500     END-EVALUATE                                                         
062600                                                                          
062700     MOVE ZERO TO RETURN-CODE                                             
062800     GOBACK                                                               
062900     .                                                                    
063000     SKIP2                                                                
063100 A-SOK-DISTR-GE-MARKNAD SECTION.                                          
063200                                                                          
063300     SET IX TO 1                                                          
063400     SEARCH DISTRIKT                                                      
063500       AT END                                                             
063600         MOVE  '1'                            TO MARK-KDSVAR              
063700       WHEN MARK-IDDISTR >= DISTR-FROM(IX) AND <= DISTR-TOM (IX)          
063800         MOVE MARKNAD-96(IX)                  TO MARK-KDMARK-BUDG         
063900     END-SEARCH                                                           
064000                                                                          
064100     IF MARK-KDSVAR = '1'                                                 
064200       MOVE +95                               TO MARK-KDMARK-BUDG         
064300     END-IF                                                               
064400     MOVE BEMARKNAD-96(MARK-KDMARK-BUDG)      TO MARK-BEMARKN             
064500     .                                                                    
064600     SKIP2                                                                
064700 B-SOK-MARKN96-GE-BEN SECTION.                                            
064800                                                                          
064900     IF MARK-KDMARK-BUDG < +1 OR > +96                                    
065000       MOVE '2'                               TO MARK-KDSVAR              
065100     ELSE                                                                 
065200       MOVE BEMARKNAD-96(MARK-KDMARK-BUDG)    TO MARK-BEMARKN             
065300     END-IF                                                               
065400     .                                                                    
