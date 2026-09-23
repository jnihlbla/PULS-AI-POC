000010*** EDIT ALLOWED                                                          
000011*                            *************************************        
000012*                            *** ANVÄNDS VID TEST AV:                     
000013*                            ***  - HANTERING AV EMBALLAGE.               
000014*                            ***                                          
000015*                            ***  - DIST04-EMB-SE: SAMMA MOT-DC           
000016*                            ***    OAVSETT KUND (ANV. TABEL-1)           
000017*                            ***                                          
000018*                            ***  - DIST04-EMB-EXP: MOT-DC BEROR          
000019*                            ***    AV VILKEN KUND (ANV.TABEL-2)          
000020*                            ***                                          
000021*                            ***  - TABELL-1:                             
000022*                            ***    SÖK MOTTAGANDE DC MHA                 
000023*                            ***    DISTRIKT.                             
000024*                            ***                                          
000025*                            ***  - TABELL-2:                             
000026*                            ***    SÖK MOTTAGANDE DC MHA                 
000027*                            ***    DISTRIKT/KND.                         
000028*                            ***                                          
000029*                            ***    ANVÄND SEARCH ALL FÖR ATT             
000030*                            ***    SÖKA I TABELLER.                      
000031*                            ***                                          
000032*                            ***  OBS!!!                                  
000033*                            ***    TABELLER MÅSTE VARA SORTERADE         
000034*                            ***    ANPASSA OCCURS NN LÄNGST NER!!        
000035*                            *************************************        
000036*                                                                         
000037 01  DIST04-IDDISTR-SE       PIC 9(5)    COMP-3.                          
000038       88  DIST04-EMB-SE       VALUE    8032 8037 8062                    
000039                                        8072 8082 8092                    
000040                                        8141 THRU 8149                    
000041                                        8700 THRU 8704                    
000043                                        8802                              
000044                                        8807 THRU 8812                    
000045                                        8851 THRU 8868.                   
000046                                                                          
000047*   TABELL-1 FÖR ATT SÖKA MOT-DC FÖR MHA DISTRIKT.                        
000048*                                                                         
000049 01  DIST04-TAB1-DIST-DC.                                                 
000050     03  FILLER             PIC X(8) VALUE '08032 21'.                    
000051     03  FILLER             PIC X(8) VALUE '08037 21'.                    
000060     03  FILLER             PIC X(8) VALUE '08062 24'.                    
000070     03  FILLER             PIC X(8) VALUE '08072 25'.                    
000080     03  FILLER             PIC X(8) VALUE '08082 26'.                    
000090     03  FILLER             PIC X(8) VALUE '08092 3A'.                    
000100     03  FILLER             PIC X(8) VALUE '08141 41'.                    
000300     03  FILLER             PIC X(8) VALUE '08143 43'.                    
000400     03  FILLER             PIC X(8) VALUE '08144 44'.                    
000410     03  FILLER             PIC X(8) VALUE '08145 45'.                    
000420     03  FILLER             PIC X(8) VALUE '08146 46'.                    
000421     03  FILLER             PIC X(8) VALUE '08147 47'.                    
000422     03  FILLER             PIC X(8) VALUE '08148 48'.                    
000423     03  FILLER             PIC X(8) VALUE '08149 49'.                    
000430     03  FILLER             PIC X(8) VALUE '08700 1A'.                    
000440     03  FILLER             PIC X(8) VALUE '08701 1B'.                    
000441     03  FILLER             PIC X(8) VALUE '08702 1C'.                    
000450     03  FILLER             PIC X(8) VALUE '08703 1D'.                    
000460     03  FILLER             PIC X(8) VALUE '08704 1E'.                    
000470     03  FILLER             PIC X(8) VALUE '08802 2C'.                    
000480     03  FILLER             PIC X(8) VALUE '08807 2H'.                    
000490     03  FILLER             PIC X(8) VALUE '08808 2I'.                    
000500     03  FILLER             PIC X(8) VALUE '08809 2J'.                    
000600     03  FILLER             PIC X(8) VALUE '08811 2L'.                    
000700     03  FILLER             PIC X(8) VALUE '08812 2M'.                    
000800     03  FILLER             PIC X(8) VALUE '08851 3B'.                    
000900     03  FILLER             PIC X(8) VALUE '08852 3C'.                    
001000     03  FILLER             PIC X(8) VALUE '08853 3D'.                    
001100     03  FILLER             PIC X(8) VALUE '08854 3E'.                    
001200     03  FILLER             PIC X(8) VALUE '08855 3F'.                    
001210     03  FILLER             PIC X(8) VALUE '08856 3G'.                    
001220     03  FILLER             PIC X(8) VALUE '08857 3H'.                    
001221     03  FILLER             PIC X(8) VALUE '08859 3J'.                    
001222     03  FILLER             PIC X(8) VALUE '08860 3K'.                    
001223     03  FILLER             PIC X(8) VALUE '08861 3L'.                    
001224     03  FILLER             PIC X(8) VALUE '08862 3M'.                    
001225     03  FILLER             PIC X(8) VALUE '08863 3N'.                    
001226     03  FILLER             PIC X(8) VALUE '08864 3O'.                    
001227     03  FILLER             PIC X(8) VALUE '08865 3P'.                    
001228     03  FILLER             PIC X(8) VALUE '08866 3R'.                    
001229     03  FILLER             PIC X(8) VALUE '08867 3S'.                    
001230     03  FILLER             PIC X(8) VALUE '08868 3T'.                    
001240*                                                                         
001250 01  DIST04-DIST-DC-TAB REDEFINES DIST04-TAB1-DIST-DC.                    
001260     03  DIST04-DIST-DC OCCURS 42 TIMES                                   
001270                          ASCENDING KEY IS DIST04-SOK-IDDISTR             
001280                          INDEXED BY DIST04-IX1.                          
001290       05  DIST04-SOK-IDDISTR  PIC 9(5).                                  
001300       05  FILLER              PIC X(1).                                  
001400       05  DIST04-IDDC-REC     PIC X(2).                                  
001500*                                                                         
001600**********************************************************                
001700*                                                                         
001800 01  DIST04-IDDISTR-EXP      PIC 9(5)    COMP-3.                          
001900       88  DIST04-EMB-EXP      VALUE    8033 THRU 8035                    
002000                                        8063 THRU 8065                    
002100                                        8073 THRU 8075                    
002200                                        8083 THRU 8085                    
002300                                        8093 THRU 8095                    
002400                                        8111                              
002500                                        8211                              
002600                                        8402                              
002700                                        8341 THRU 8349                    
002800                                        8407 THRU 8411                    
002900                                        8440 THRU 8445                    
003000                                        8450 8452 8454                    
003100                                        8780 8790                         
003200                                        8880 8887 8888                    
003210                                        8890 8897 8899.                   
003220                                                                          
003230*   TABELL-2 FÖR ATT SÖKA MOT-DC FÖR MHA DISTRIKT/KUND.                   
003240*                                                                         
003250 01  DIST04-TAB2-DIST-KUND-DC.                                            
003260     03  FILLER             PIC X(15) VALUE '080330000017 11'.            
003270     03  FILLER             PIC X(15) VALUE '080330000031 11'.            
003280     03  FILLER             PIC X(15) VALUE '080330000044 11'.            
003290     03  FILLER             PIC X(15) VALUE '080340000017 11'.            
003291     03  FILLER             PIC X(15) VALUE '080340000031 11'.            
003292     03  FILLER             PIC X(15) VALUE '080340000044 11'.            
003293     03  FILLER             PIC X(15) VALUE '080350000024 21'.            
003294     03  FILLER             PIC X(15) VALUE '080350000025 21'.            
003295     03  FILLER             PIC X(15) VALUE '080350000026 21'.            
003296     03  FILLER             PIC X(15) VALUE '080350001000 21'.            
003297     03  FILLER             PIC X(15) VALUE '080350001001 21'.            
003298     03  FILLER             PIC X(15) VALUE '080350001002 21'.            
003299     03  FILLER             PIC X(15) VALUE '080350002008 21'.            
003300     03  FILLER             PIC X(15) VALUE '080350002009 21'.            
003400     03  FILLER             PIC X(15) VALUE '080350002011 21'.            
003500     03  FILLER             PIC X(15) VALUE '080350002012 21'.            
003600     03  FILLER             PIC X(15) VALUE '080350003000 21'.            
003700     03  FILLER             PIC X(15) VALUE '080350003001 21'.            
003800     03  FILLER             PIC X(15) VALUE '080350003002 21'.            
003900     03  FILLER             PIC X(15) VALUE '080350003003 21'.            
004000     03  FILLER             PIC X(15) VALUE '080350003004 21'.            
004100     03  FILLER             PIC X(15) VALUE '080350003006 21'.            
004200     03  FILLER             PIC X(15) VALUE '080350003010 21'.            
004300     03  FILLER             PIC X(15) VALUE '080350003011 21'.            
004400     03  FILLER             PIC X(15) VALUE '080350003012 21'.            
004500     03  FILLER             PIC X(15) VALUE '080350003013 21'.            
004600     03  FILLER             PIC X(15) VALUE '080350003015 21'.            
004700     03  FILLER             PIC X(15) VALUE '080350003016 21'.            
004800     03  FILLER             PIC X(15) VALUE '080350003017 21'.            
004900     03  FILLER             PIC X(15) VALUE '080350003018 21'.            
004910     03  FILLER             PIC X(15) VALUE '080390003001 21'.            
004920     03  FILLER             PIC X(15) VALUE '080390003002 21'.            
005000     03  FILLER             PIC X(15) VALUE '080630000017 11'.            
005100     03  FILLER             PIC X(15) VALUE '080630000031 11'.            
005200     03  FILLER             PIC X(15) VALUE '080630000044 11'.            
005300     03  FILLER             PIC X(15) VALUE '080640000017 11'.            
005400     03  FILLER             PIC X(15) VALUE '080640000031 11'.            
005500     03  FILLER             PIC X(15) VALUE '080640000044 11'.            
005600     03  FILLER             PIC X(15) VALUE '080650000021 24'.            
005610     03  FILLER             PIC X(15) VALUE '080650003016 24'.            
005620     03  FILLER             PIC X(15) VALUE '080730000017 11'.            
005630     03  FILLER             PIC X(15) VALUE '080730000031 11'.            
005640     03  FILLER             PIC X(15) VALUE '080730000044 11'.            
005650     03  FILLER             PIC X(15) VALUE '080740000017 11'.            
005660     03  FILLER             PIC X(15) VALUE '080740000031 11'.            
005670     03  FILLER             PIC X(15) VALUE '080740000044 11'.            
005680     03  FILLER             PIC X(15) VALUE '080750003003 25'.            
005690     03  FILLER             PIC X(15) VALUE '080750003005 25'.            
005700     03  FILLER             PIC X(15) VALUE '080830000017 11'.            
005800     03  FILLER             PIC X(15) VALUE '080830000031 11'.            
005810     03  FILLER             PIC X(15) VALUE '080830000044 11'.            
005820     03  FILLER             PIC X(15) VALUE '080840000017 11'.            
005830     03  FILLER             PIC X(15) VALUE '080840000031 11'.            
005840     03  FILLER             PIC X(15) VALUE '080840000044 11'.            
005850     03  FILLER             PIC X(15) VALUE '080850000021 26'.            
005860     03  FILLER             PIC X(15) VALUE '080850003016 26'.            
005870     03  FILLER             PIC X(15) VALUE '080930003000 11'.            
005880     03  FILLER             PIC X(15) VALUE '080940003000 11'.            
005890     03  FILLER             PIC X(15) VALUE '080950000021 3A'.            
005900     03  FILLER             PIC X(15) VALUE '080950002002 3A'.            
006000     03  FILLER             PIC X(15) VALUE '080950003001 3A'.            
006100     03  FILLER             PIC X(15) VALUE '080950003016 3A'.            
006200     03  FILLER             PIC X(15) VALUE '081110000041 11'.            
006210     03  FILLER             PIC X(15) VALUE '081110000042 11'.            
006220     03  FILLER             PIC X(15) VALUE '081110000043 11'.            
006230     03  FILLER             PIC X(15) VALUE '081110000044 11'.            
006240     03  FILLER             PIC X(15) VALUE '081110000045 11'.            
006250     03  FILLER             PIC X(15) VALUE '081110000046 11'.            
006251     03  FILLER             PIC X(15) VALUE '081110000047 11'.            
006260     03  FILLER             PIC X(15) VALUE '082110000041 11'.            
006261     03  FILLER             PIC X(15) VALUE '082110000042 11'.            
006262     03  FILLER             PIC X(15) VALUE '082110000043 11'.            
006263     03  FILLER             PIC X(15) VALUE '082110000044 11'.            
006264     03  FILLER             PIC X(15) VALUE '082110000045 11'.            
006265     03  FILLER             PIC X(15) VALUE '082110000046 11'.            
006266     03  FILLER             PIC X(15) VALUE '082110000047 11'.            
006267     03  FILLER             PIC X(15) VALUE '083410000042 41'.            
006268     03  FILLER             PIC X(15) VALUE '083410000043 41'.            
006269     03  FILLER             PIC X(15) VALUE '083410000044 41'.            
006270     03  FILLER             PIC X(15) VALUE '083410000045 41'.            
006271     03  FILLER             PIC X(15) VALUE '083410000046 41'.            
006272     03  FILLER             PIC X(15) VALUE '083410000047 41'.            
006276     03  FILLER             PIC X(15) VALUE '083430000041 43'.            
006277     03  FILLER             PIC X(15) VALUE '083430000042 43'.            
006278     03  FILLER             PIC X(15) VALUE '083430000044 43'.            
006279     03  FILLER             PIC X(15) VALUE '083430000045 43'.            
006280     03  FILLER             PIC X(15) VALUE '083430000046 43'.            
006281     03  FILLER             PIC X(15) VALUE '083430000047 43'.            
006282     03  FILLER             PIC X(15) VALUE '083440000041 44'.            
006283     03  FILLER             PIC X(15) VALUE '083440000042 44'.            
006284     03  FILLER             PIC X(15) VALUE '083440000043 44'.            
006285     03  FILLER             PIC X(15) VALUE '083440000045 44'.            
006286     03  FILLER             PIC X(15) VALUE '083440000046 44'.            
006287     03  FILLER             PIC X(15) VALUE '083440000047 44'.            
006288     03  FILLER             PIC X(15) VALUE '083450000041 45'.            
006289     03  FILLER             PIC X(15) VALUE '083450000042 45'.            
006290     03  FILLER             PIC X(15) VALUE '083450000043 45'.            
006291     03  FILLER             PIC X(15) VALUE '083450000044 45'.            
006292     03  FILLER             PIC X(15) VALUE '083450000046 45'.            
006293     03  FILLER             PIC X(15) VALUE '083450000047 45'.            
006294     03  FILLER             PIC X(15) VALUE '083460000041 46'.            
006295     03  FILLER             PIC X(15) VALUE '083460000042 46'.            
006296     03  FILLER             PIC X(15) VALUE '083460000043 46'.            
006297     03  FILLER             PIC X(15) VALUE '083460000044 46'.            
006298     03  FILLER             PIC X(15) VALUE '083460000045 46'.            
006299     03  FILLER             PIC X(15) VALUE '083460000047 46'.            
006300     03  FILLER             PIC X(15) VALUE '083470000041 47'.            
006301     03  FILLER             PIC X(15) VALUE '083470000042 47'.            
006302     03  FILLER             PIC X(15) VALUE '083470000043 47'.            
006303     03  FILLER             PIC X(15) VALUE '083470000044 47'.            
006304     03  FILLER             PIC X(15) VALUE '083470000045 47'.            
006305     03  FILLER             PIC X(15) VALUE '083470000046 47'.            
006306     03  FILLER             PIC X(15) VALUE '084020003000 2C'.            
006307     03  FILLER             PIC X(15) VALUE '084070003001 2H'.            
006308     03  FILLER             PIC X(15) VALUE '084080000021 2I'.            
006309     03  FILLER             PIC X(15) VALUE '084080003002 2I'.            
006310     03  FILLER             PIC X(15) VALUE '084080003016 2I'.            
006311     03  FILLER             PIC X(15) VALUE '084090003004 2J'.            
006312     03  FILLER             PIC X(15) VALUE '084110000021 2L'.            
006313     03  FILLER             PIC X(15) VALUE '084110003006 2L'.            
006314     03  FILLER             PIC X(15) VALUE '084110003016 2L'.            
006315     03  FILLER             PIC X(15) VALUE '084400000021 3B'.            
006316     03  FILLER             PIC X(15) VALUE '084400002007 3B'.            
006317     03  FILLER             PIC X(15) VALUE '084400003000 3B'.            
006318     03  FILLER             PIC X(15) VALUE '084400003016 3B'.            
006319     03  FILLER             PIC X(15) VALUE '084410000021 3C'.            
006320     03  FILLER             PIC X(15) VALUE '084410002008 3C'.            
006321     03  FILLER             PIC X(15) VALUE '084410002009 3C'.            
006322     03  FILLER             PIC X(15) VALUE '084410003004 3C'.            
006323     03  FILLER             PIC X(15) VALUE '084410003016 3C'.            
006324     03  FILLER             PIC X(15) VALUE '084420000025 3D'.            
006325     03  FILLER             PIC X(15) VALUE '084420003005 3D'.            
006326     03  FILLER             PIC X(15) VALUE '084430000021 3E'.            
006327     03  FILLER             PIC X(15) VALUE '084430002009 3E'.            
006328     03  FILLER             PIC X(15) VALUE '084430003016 3E'.            
006329     03  FILLER             PIC X(15) VALUE '084440000025 3F'.            
006330     03  FILLER             PIC X(15) VALUE '084440003003 3F'.            
006331     03  FILLER             PIC X(15) VALUE '084450000021 3G'.            
006332     03  FILLER             PIC X(15) VALUE '084450002011 3G'.            
006333     03  FILLER             PIC X(15) VALUE '084450003016 3G'.            
006334     03  FILLER             PIC X(15) VALUE '084500000021 3L'.            
006335     03  FILLER             PIC X(15) VALUE '084500003016 3L'.            
006336     03  FILLER             PIC X(15) VALUE '084520000021 3N'.            
006337     03  FILLER             PIC X(15) VALUE '084520002012 3N'.            
006338     03  FILLER             PIC X(15) VALUE '084520003016 3N'.            
006339     03  FILLER             PIC X(15) VALUE '084540000021 3P'.            
006340     03  FILLER             PIC X(15) VALUE '084540003016 3P'.            
006341     03  FILLER             PIC X(15) VALUE '087800001000 11'.            
006342     03  FILLER             PIC X(15) VALUE '087800001001 11'.            
006343     03  FILLER             PIC X(15) VALUE '087800001003 11'.            
006344     03  FILLER             PIC X(15) VALUE '087800001004 11'.            
006345     03  FILLER             PIC X(15) VALUE '087900001000 11'.            
006346     03  FILLER             PIC X(15) VALUE '087900001001 11'.            
006347     03  FILLER             PIC X(15) VALUE '087900001003 11'.            
006348     03  FILLER             PIC X(15) VALUE '087900001004 11'.            
006349     03  FILLER             PIC X(15) VALUE '088800002002 11'.            
006350     03  FILLER             PIC X(15) VALUE '088800002007 11'.            
006351     03  FILLER             PIC X(15) VALUE '088800002008 11'.            
006352     03  FILLER             PIC X(15) VALUE '088800002009 11'.            
006353     03  FILLER             PIC X(15) VALUE '088800002011 11'.            
006354     03  FILLER             PIC X(15) VALUE '088800002012 11'.            
006355     03  FILLER             PIC X(15) VALUE '088800003001 11'.            
006356     03  FILLER             PIC X(15) VALUE '088800003002 11'.            
006357     03  FILLER             PIC X(15) VALUE '088800003003 11'.            
006358     03  FILLER             PIC X(15) VALUE '088800003004 11'.            
006359     03  FILLER             PIC X(15) VALUE '088800003005 11'.            
006360     03  FILLER             PIC X(15) VALUE '088800003006 11'.            
006361     03  FILLER             PIC X(15) VALUE '088800003010 11'.            
006362     03  FILLER             PIC X(15) VALUE '088800003011 11'.            
006363     03  FILLER             PIC X(15) VALUE '088800003012 11'.            
006364     03  FILLER             PIC X(15) VALUE '088800003013 11'.            
006365     03  FILLER             PIC X(15) VALUE '088800003014 11'.            
006366     03  FILLER             PIC X(15) VALUE '088800003015 11'.            
006367     03  FILLER             PIC X(15) VALUE '088800003016 11'.            
006368     03  FILLER             PIC X(15) VALUE '088800003017 11'.            
006369     03  FILLER             PIC X(15) VALUE '088800003018 11'.            
006370     03  FILLER             PIC X(15) VALUE '088860002002 11'.            
006371     03  FILLER             PIC X(15) VALUE '088860002007 11'.            
006372     03  FILLER             PIC X(15) VALUE '088860003000 11'.            
006373     03  FILLER             PIC X(15) VALUE '088860003001 11'.            
006374     03  FILLER             PIC X(15) VALUE '088870003009 11'.            
006375     03  FILLER             PIC X(15) VALUE '088880003007 11'.            
006376     03  FILLER             PIC X(15) VALUE '088900002002 11'.            
006377     03  FILLER             PIC X(15) VALUE '088900002007 11'.            
006378     03  FILLER             PIC X(15) VALUE '088900002008 11'.            
006379     03  FILLER             PIC X(15) VALUE '088900002009 11'.            
006380     03  FILLER             PIC X(15) VALUE '088900002011 11'.            
006381     03  FILLER             PIC X(15) VALUE '088900002012 11'.            
006382     03  FILLER             PIC X(15) VALUE '088900003001 11'.            
006383     03  FILLER             PIC X(15) VALUE '088900003002 11'.            
006384     03  FILLER             PIC X(15) VALUE '088900003003 11'.            
006385     03  FILLER             PIC X(15) VALUE '088900003004 11'.            
006386     03  FILLER             PIC X(15) VALUE '088900003005 11'.            
006387     03  FILLER             PIC X(15) VALUE '088900003006 11'.            
006388     03  FILLER             PIC X(15) VALUE '088900003010 11'.            
006389     03  FILLER             PIC X(15) VALUE '088900003011 11'.            
006390     03  FILLER             PIC X(15) VALUE '088900003012 11'.            
006391     03  FILLER             PIC X(15) VALUE '088900003013 11'.            
006392     03  FILLER             PIC X(15) VALUE '088900003014 11'.            
006393     03  FILLER             PIC X(15) VALUE '088900003015 11'.            
006394     03  FILLER             PIC X(15) VALUE '088900003016 11'.            
006395     03  FILLER             PIC X(15) VALUE '088900003017 11'.            
006396     03  FILLER             PIC X(15) VALUE '088900003018 11'.            
006397     03  FILLER             PIC X(15) VALUE '088960002002 11'.            
006398     03  FILLER             PIC X(15) VALUE '088960002007 11'.            
006399     03  FILLER             PIC X(15) VALUE '088960003000 11'.            
006400     03  FILLER             PIC X(15) VALUE '088960003001 11'.            
006401     03  FILLER             PIC X(15) VALUE '088970003009 11'.            
006402     03  FILLER             PIC X(15) VALUE '088990003007 11'.            
006403*                                                                         
006404 01  DIST04-DISTKUND-DC-TAB REDEFINES DIST04-TAB2-DIST-KUND-DC.           
006405     03  DIST04-DIST-KUND-DC OCCURS 209 TIMES                             
006406                       ASCENDING KEY IS DIST04-SOK-DIST-KND               
006407                       INDEXED BY DIST04-IX2.                             
006408       05  DIST04-SOK-DIST-KND PIC 9(12).                                 
006409       05  FILLER REDEFINES DIST04-SOK-DIST-KND.                          
006410         07  DIST04-IDDISTR    PIC 9(5).                                  
006411         07  DIST04-IDKUNDNR   PIC 9(7).                                  
006412       05  FILLER              PIC X(1).                                  
006420       05  DIST04-IDDC-REC2    PIC X(2).                                  
006500*                                                                         
