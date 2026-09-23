000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W3725400.                                                
000004 AUTHOR.         INGVAR SKJELBRED.                                        
000005 DATE-WRITTEN.   97/09/08.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*                                                                         
000009*    FUNKTION:                                                            
000010*        PROGRAMMET RÄKNAR UT SNITTETARBETSDAGAR                          
000011* DVS TIDEN DET TAR ATT GODKÄNNA EN BYTESRAPPORT                          
000012*                                                                         
000013*    ABENDKODER:                                                          
000014*        U0016 -  . . . .                                                 
000015*        U1000 -  . . . .                                                 
000016*                                                                         
000017*    CHANGE LOG:                                                          
000018*                                                                         
000019*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
000020*      ----------------------------------------------------------         
000021*      15/04/07 - REDDY RAHUL     - CHINA EXCHANGE PHASE 2.               
000022*                                   E'TRACKER 10252358                    
000023*                                   CONSOLIDATE WEB REPORTS AND           
000024*                                   ADD CHINA REPORTS.                    
000025*      16/09/30 - ARUP DATTA      - NDC SEATTLE.          .               
000026*                                   E'TRACKER 10287310                    
000027*                                   CREATE WEB REPORT FOR DC44.           
000028*                                                                         
000029                                                                          
000030                                                                          
000031     SKIP3                                                                
000032 ENVIRONMENT DIVISION.                                                    
000033     SKIP2                                                                
000034 INPUT-OUTPUT SECTION.                                                    
000035                                                                          
000036 FILE-CONTROL.                                                            
000037     SKIP2                                                                
000038*          --- UNDERLAG TILL BERÄKNING AV SNITTUNDERLAGET                 
000039     SELECT W3724D                     ASSIGN TO W37254D1.                
000040     SKIP2                                                                
000041*          --- LISTOR TILL BYTES DC 11 91 41 43                           
000042     SELECT LISTA                      ASSIGN TO W37254D2.                
000043     SELECT LISTB                      ASSIGN TO W37254D3.                
000044     SELECT LISTC                      ASSIGN TO W37254D4.                
000045     SELECT LISTE                      ASSIGN TO W37254D5.                
000046     SELECT LISTF                      ASSIGN TO W37254D6.                
000047     SELECT LISTG                      ASSIGN TO W37254D7.                
000048     EJECT                                                                
000049 DATA DIVISION.                                                           
000050     SKIP3                                                                
000051 FILE SECTION.                                                            
000052     SKIP3                                                                
000053 FD  W3724D                                                               
000054     RECORDING       F                                                    
000055     BLOCK CONTAINS  0.                                                   
000056                                                                          
000057*01  -COPY W3714D      -L.                                                
000058     SKIP3                                                                
000059 FD  LISTA                                                                
000060     RECORDING       F                                                    
000061     BLOCK CONTAINS  0.                                                   
000062     SKIP2                                                                
000063 01  LISTAS                      PIC X(121).                              
000064     SKIP3                                                                
000065 FD  LISTB                                                                
000066     RECORDING       V                                                    
000067     BLOCK CONTAINS  0.                                                   
000068     SKIP2                                                                
000069 01  LISTBS                      PIC X(125).                              
000070     EJECT                                                                
000071     SKIP3                                                                
000072 FD  LISTC                                                                
000073     RECORDING       F                                                    
000074     BLOCK CONTAINS  0.                                                   
000075     SKIP2                                                                
000076 01  LISTCS                      PIC X(121).                              
000077     EJECT                                                                
000078 FD  LISTE                                                                
000079     RECORDING       F                                                    
000080     BLOCK CONTAINS  0.                                                   
000081     SKIP2                                                                
000082 01  LISTES                      PIC X(121).                              
000083     EJECT                                                                
000084 FD  LISTF                                                                
000085     RECORDING       F                                                    
000086     BLOCK CONTAINS  0.                                                   
000087     SKIP2                                                                
000088 01  LISTFS                      PIC X(121).                              
000089     EJECT                                                                
000090 FD  LISTG                                                                
000091     RECORDING       V                                                    
000092     BLOCK CONTAINS  0.                                                   
000093     SKIP2                                                                
000094 01  LISTGS                      PIC X(125).                              
000095     EJECT                                                                
000096 WORKING-STORAGE SECTION.                                                 
000097                                                                          
000098*    -- CHECKED BY WY2000                                                 
000099 77  IDPGM                       PIC X(8)    VALUE 'W3725400'.            
000100 77   PROGRAM-NAMN           VALUE 'W3725400'                             
000101                                 PIC X(8).                                
000102 77  JA                          PIC X       VALUE 'J'.                   
000103 77  NEJ                         PIC X       VALUE 'N'.                   
000104                                                                          
000105 77  WS-SPAR-IDDC                PIC X(2)    VALUE SPACE.                 
000106                                                                          
000107 77  W-KVRETUR11                 PIC S9(7)   VALUE +0 COMP-3.             
000108 77  W-KVRETUR11-W               PIC S9(7)   VALUE +0 COMP-3.             
000109 77  W-KVRETUR91                 PIC S9(7)   VALUE +0 COMP-3.             
000110 77  W-KVRETUR41                 PIC S9(7)   VALUE +0 COMP-3.             
000111 77  W-KVRETUR41-W               PIC S9(7)   VALUE +0 COMP-3.             
000112 77  W-KVRETUR43                 PIC S9(7)   VALUE +0 COMP-3.             
000113 77  W-KVRETUR61                 PIC S9(7)   VALUE +0 COMP-3.             
000114 77  W-KVRETUR61-W               PIC S9(7)   VALUE +0 COMP-3.             
000115 77  W-KVRETURDC                 PIC S9(7)   VALUE +0 COMP-3.             
000116 77  W-KVRETURDC-W               PIC S9(7)   VALUE +0 COMP-3.             
000117 77  W-ANTALPOSTER               PIC S9(7)   VALUE +0 COMP-3.             
000118 77  W-ANTALPOSTER-W             PIC S9(7)   VALUE +0 COMP-3.             
000119 77  W-ANTALPOSTER2              PIC S9(7)   VALUE +0 COMP-3.             
000120 77  W-ANTALPOSTER41             PIC S9(7)   VALUE +0 COMP-3.             
000121 77  W-ANTALPOSTER41-W           PIC S9(7)   VALUE +0 COMP-3.             
000122 77  W-ANTALPOSTER43             PIC S9(7)   VALUE +0 COMP-3.             
000123 77  W-ANTALPOSTER61             PIC S9(7)   VALUE +0 COMP-3.             
000124 77  W-ANTALPOSTER61-W           PIC S9(7)   VALUE +0 COMP-3.             
000125 77  W-ANTALPOSTERDC             PIC S9(7)   VALUE +0 COMP-3.             
000126 77  W-ANTALPOSTERDC-W           PIC S9(7)   VALUE +0 COMP-3.             
000127 77  W-ANTALSUMMA                PIC S9(7)V99 VALUE +0 COMP-3.            
000128 77  W-ANTALSUMMA-W              PIC S9(7)V99 VALUE +0 COMP-3.            
000129 77  W-ANTALSUMMA2               PIC S9(7)V99 VALUE +0 COMP-3.            
000130 77  W-ANTALSUMMA41              PIC S9(7)V99 VALUE +0 COMP-3.            
000131 77  W-ANTALSUMMA41-W            PIC S9(7)V99 VALUE +0 COMP-3.            
000132 77  W-ANTALSUMMA43              PIC S9(7)V99 VALUE +0 COMP-3.            
000133 77  W-ANTALSUMMA61              PIC S9(7)V99 VALUE +0 COMP-3.            
000134 77  W-ANTALSUMMA61-W            PIC S9(7)V99 VALUE +0 COMP-3.            
000135 77  W-ANTALSUMMADC              PIC S9(7)V99 VALUE +0 COMP-3.            
000136 77  W-ANTALSUMMADC-W            PIC S9(7)V99 VALUE +0 COMP-3.            
000137 77  W-KVARBDAG                  PIC S9(7)   VALUE +0 COMP-3.             
000138 77  W-KVARBDAG-W                PIC S9(7)   VALUE +0 COMP-3.             
000139 77  W-KVARBDAG2                 PIC S9(7)   VALUE +0 COMP-3.             
000140 77  W-KVARBDAG41                PIC S9(7)   VALUE +0 COMP-3.             
000141 77  W-KVARBDAG41-W              PIC S9(7)   VALUE +0 COMP-3.             
000142 77  W-KVARBDAG43                PIC S9(7)   VALUE +0 COMP-3.             
000143 77  W-KVARBDAG61                PIC S9(7)   VALUE +0 COMP-3.             
000144 77  W-KVARBDAG61-W              PIC S9(7)   VALUE +0 COMP-3.             
000145 77  W-KVARBDAGDC                PIC S9(7)   VALUE +0 COMP-3.             
000146 77  W-KVARBDAGDC-W              PIC S9(7)   VALUE +0 COMP-3.             
000147                                                                          
000148 77  W3724D-EOF-SW               PIC X       VALUE 'N'.                   
000149     88  END-OF-W3724D                       VALUE 'J'.                   
000150     EJECT                                                                
000151     EJECT                                                                
000152 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000153 01  FILLER REDEFINES DAGENS-DATUM.                                       
000154     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000155     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000156     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000157     EJECT                                                                
000158                                                                          
000159 01  WS-DATUM.                                                            
000160     03  DATUM-SEKEL             PIC X(2).                                
000161     03  DATUM-AAR               PIC X(2).                                
000162     03  FILLER                  PIC X(1)    VALUE '-'.                   
000163     03  DATUM-MAANAD            PIC X(2).                                
000164     03  FILLER                  PIC X(1)    VALUE '-'.                   
000165     03  DATUM-DAG               PIC X(2).                                
000166                                                                          
000167                                                                          
000168 01  WS-DATUM-GB.                                                         
000169     03  DATUM-GB-DAG            PIC X(2).                                
000170     03  FILLER                  PIC X(1)    VALUE '-'.                   
000171     03  DATUM-GB-MAANAD         PIC X(2).                                
000172     03  FILLER                  PIC X(1)    VALUE '-'.                   
000173     03  DATUM-GB-SEKEL          PIC X(2).                                
000174     03  DATUM-GB-AAR            PIC X(2).                                
000175                                                                          
000176                                                                          
000177                                                                          
000178 01  WS-DATUM-US.                                                         
000179     03  DATUM-US-MAANAD         PIC X(2).                                
000180     03  FILLER                  PIC X(1)    VALUE '-'.                   
000181     03  DATUM-US-DAG            PIC X(2).                                
000182     03  FILLER                  PIC X(1)    VALUE '-'.                   
000183     03  DATUM-US-SEKEL          PIC X(2).                                
000184     03  DATUM-US-AAR            PIC X(2).                                
000185                                                                          
000186     EJECT                                                                
000187 01  DYNAMISKA-SUBPROGRAM.                                                
000188*                                                                         
000189     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000190     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000191     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000192     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000193     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
000194     SKIP2                                                                
000195*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
000196 01  FILLER                      PIC X(16)   VALUE 'DATKORT'.             
000197 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
000198     SKIP2                                                                
000199*01  -COPY WDATKORT                                                       
000200*    -- VALID IDDC CODES                                                  
000201*                                                                         
000202*01  -COPY WWDC99                                                         
000203*                                                                         
000204*    --- PARAMETRAR TILL WL10WBDC                                         
000205*                                                                         
000206*01  -COPY WL10WBDC                                                       
000207*                                                                         
000208*    --- PARAMETRAR TILL ABEND                                            
000209                                                                          
000210 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000211 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000212 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000213     SKIP2                                                                
000214 01  FELTEXT.                                                             
000215     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000216     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000217     EJECT                                                                
000218*    --- PARAMETRAR TILL POSTSUM                                          
000219*                                                                         
000220*01  -COPY W0005   -PRE  POSTSUM-                                         
000221     EJECT                                                                
000222*01  -COPY WDATAREA                                                       
000223     EJECT                                                                
000224 01  IN-AREA-START               PIC X(24)   VALUE                        
000225                                 'IN-AREA-START  '.                       
000226     SKIP2                                                                
000227                                                                          
000228*01  AREA -COPY W3714D     -PRE IN-                                       
000229     EJECT                                                                
000230 01  W001-AREA-START             PIC X(24)   VALUE                        
000231                                 'LIST-AREA-START  '.                     
000232     SKIP2                                                                
000233 01  UT-RAD                      PIC X(121)  VALUE SPACE.                 
000234 01  UT-RAD-91                   PIC X(121)  VALUE SPACE.                 
000235 01  UT-RAD-41                   PIC X(121)  VALUE SPACE.                 
000236 01  UT-RAD-43                   PIC X(121)  VALUE SPACE.                 
000237 01  UT-RAD-61                   PIC X(121)  VALUE SPACE.                 
000238 01  UT-RAD-DC                   PIC X(121)  VALUE SPACE.                 
000239*                                                                         
000240     EJECT                                                                
000250 01  W001R1-RUBRIK-S.                                                     
000251*                                                                         
000252     03  W001R1-STYR           PIC X(1)   VALUE '1'.                      
000253     03  FILLER                PIC X(2)   VALUE SPACE.                    
000254     03  FILLER                PIC X(15)  VALUE 'W37254-011'.             
000255     03  FILLER                PIC X(34)                                  
000256                  VALUE 'SNITTLEDTID FÖR BYTESRETURER FRÅN '.             
000257     03  FILLER                PIC X(39)                                  
000258                  VALUE 'STATUS 3 TILL STATUS 4 WECKO LISTA'.             
000259     03  FILLER                PIC X(7)   VALUE 'DATUM'.                  
000260     03  W001R1-DAGENS-DATUM   PIC X(10)  VALUE SPACE.                    
000261     03  FILLER                PIC X(5)   VALUE SPACE.                    
000262     03  FILLER                PIC X(7)  VALUE 'SIDA  1'.                 
000263     EJECT                                                                
000264 01  W001R1-RUBRIK-S91.                                                   
000265*                                                                         
000266*    03  W001R1-STYR-91        PIC X(1)   VALUE '1'.                      
000267*    03  FILLER                PIC X(2)   VALUE SPACE.                    
000268     03  FILLER                PIC X(10)  VALUE 'W37254-091'.             
000269     03  FILLER                PIC X(1)   VALUE ';'.                      
000270     03  FILLER                PIC X(34)                                  
000271                  VALUE 'AVERAGE LEADTIME ON EXCH.RETURNS.;'.             
000272     03  FILLER                PIC X(32)                                  
000273                  VALUE 'FROM STATUS 3 TO 4 WEEKLY LIST;'.                
000274     03  FILLER                PIC X(8)   VALUE 'DATE;'.                  
000275     03  W001R1-DAGENS-DATUM91 PIC X(10).                                 
000276     03  FILLER                PIC X      VALUE ';'.                      
000277     EJECT                                                                
000278                                                                          
000279****  USA RUBRIKER ****                                                   
000280                                                                          
000281 01  W001R1-RUBRIK-S41.                                                   
000282*                                                                         
000283     03  W001R1-STYR-41        PIC X(1)   VALUE '1'.                      
000284     03  FILLER                PIC X(2)   VALUE SPACE.                    
000285     03  FILLER                PIC X(10)  VALUE 'W37254-041'.             
000286     03  FILLER                PIC X(2)   VALUE SPACE.                    
000287     03  FILLER                PIC X(34)                                  
000288                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
000289     03  FILLER                PIC X(40)                                  
000290                  VALUE 'E A CORE WEEKLY LIST         '.                  
000291     03  FILLER                PIC X(3)   VALUE SPACE.                    
000292     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
000293     03  W001R1-DAGENS-DATUM41 PIC X(10).                                 
000294     03  FILLER                PIC X(5)   VALUE SPACE.                    
000295     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
000296                                                                          
000297                                                                          
000298 01  W001R1-RUBRIK-S43.                                                   
000299*                                                                         
000300     03  W001R1-STYR-43        PIC X(1)   VALUE '1'.                      
000310     03  FILLER                PIC X(2)   VALUE SPACE.                    
000311     03  FILLER                PIC X(15)  VALUE 'W37254-043'.             
000312     03  FILLER                PIC X(34)                                  
000313                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
000314     03  FILLER                PIC X(40)                                  
000315                  VALUE 'E A CORE WEEKLY LIST         '.                  
000316     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
000317     03  W001R1-DAGENS-DATUM43 PIC X(10).                                 
000318     03  FILLER                PIC X(5)   VALUE SPACE.                    
000319     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
000320                                                                          
000321****  JAPAN RUBRIKER ****                                                 
000322 01  W001R1-RUBRIK-S61.                                                   
000323*                                                                         
000324     03  W001R1-STYR-61        PIC X(1)   VALUE '1'.                      
000325     03  FILLER                PIC X(2)   VALUE SPACE.                    
000326     03  FILLER                PIC X(10)  VALUE 'W37254-061'.             
000327     03  FILLER                PIC X(2)   VALUE SPACE.                    
000328     03  FILLER                PIC X(34)                                  
000329                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
000330     03  FILLER                PIC X(40)                                  
000331                  VALUE 'E A CORE WEEKLY LIST          '.                 
000332     03  FILLER                PIC X(3)   VALUE SPACE.                    
000333     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
000334     03  W001R1-DAGENS-DATUM61 PIC X(10).                                 
000335     03  FILLER                PIC X(5)   VALUE SPACE.                    
000336     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
000337                                                                          
000338     EJECT                                                                
000339                                                                          
000340****  WEB DC RUBRIKER ****                                                
000341 01  W001R1-RUBRIK-DC.                                                    
000342*                                                                         
000343     03  FILLER                PIC X(3)   VALUE SPACE.                    
000344     03  FILLER                PIC X(10)  VALUE 'W37254-001'.             
000345     03  FILLER                PIC X(2)   VALUE SPACE.                    
000346     03  FILLER                PIC X(34)                                  
000347                  VALUE 'THE WORKING TIME AVERAGE TO APPROV'.             
000348     03  FILLER                PIC X(40)                                  
000349                  VALUE 'E A CORE WEEKLY LIST          '.                 
000350     03  FILLER                PIC X(3)   VALUE SPACE.                    
000351     03  FILLER                PIC X(6)   VALUE 'DATE '.                  
000352     03  W001R1-DAGENS-DATUMDC PIC X(10).                                 
000353     03  FILLER                PIC X(5)   VALUE SPACE.                    
000354     03  FILLER                PIC X(7)  VALUE 'PAGE 1'.                  
000355                                                                          
000356     EJECT                                                                
000357                                                                          
000358                                                                          
000359 01  W001R2-DELRUBRIK-1.                                                  
000360*                                                                         
000361     03  W001R2-STYR           PIC X(1)   VALUE '0'.                      
000362     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000363     03  FILLER                PIC X(7)   VALUE 'DC '.                    
000364     03  FILLER                PIC X(6)   VALUE '     '.                  
000365     03  FILLER                PIC X(17)                                  
000366                         VALUE 'SNITT ARBETSTIDEN'.                       
000367     03  FILLER                PIC X(2)   VALUE SPACE.                    
000368     03  FILLER                PIC X(17)                                  
000369                         VALUE 'SNITT ARBETSTIDEN'.                       
000370     03  FILLER                PIC X(17)                                  
000371                         VALUE '  ANTAL ARTIKLAR '.                       
000372     03  FILLER                PIC X(11) VALUE SPACE.                     
000373                                                                          
000374     EJECT                                                                
000375*                                                                         
000376 01  W001R2-DELRUBRIK-3.                                                  
000377     03  FILLER                PIC X(1)   VALUE '0'.                      
000378     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000379     03  FILLER                PIC X(7)   VALUE '   '.                    
000380     03  FILLER                PIC X(6)   VALUE '     '.                  
000381     03  FILLER                PIC X(23)                                  
000382               VALUE '       EJ GARANTI      '.                           
000383     03  FILLER                PIC X(14)                                  
000384               VALUE '      GARANTI '.                                    
000385     03  FILLER                PIC X(2)   VALUE SPACE.                    
000386     03  FILLER                PIC X(4)   VALUE SPACE.                    
000387     03  FILLER                PIC X(21)  VALUE SPACE.                    
000388                                                                          
000389*                                                                         
000390 01  W001R2-DELRUBRIK-1-91.                                               
000391*                                                                         
000392*    03  W001R2-STYR-91        PIC X(1)   VALUE '0'.                      
000393*    03  FILLER                PIC X(4)   VALUE  SPACE.                   
000394     03  FILLER                PIC X(3)   VALUE 'DC;'.                    
000395     03  FILLER                PIC X(32)                                  
000396                VALUE 'WORKTIME AVERAGE;TOTAL WORKTIME;'.                 
000397     03  FILLER                PIC X(14)                                  
000398                VALUE 'TOTAL REPORTS;'.                                   
000399                                                                          
000400     EJECT                                                                
000401*                                                                         
000402                                                                          
000403 01  W001R2-DELRUBRIK-1-61.                                               
000404*                                                                         
000405     03  W001R2-STYR-61        PIC X(1)   VALUE '0'.                      
000406     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000407     03  FILLER                PIC X(7)   VALUE 'DC '.                    
000408     03  FILLER                PIC X(3)   VALUE '   '.                    
000409     03  FILLER                PIC X(26)                                  
000410               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
000411     03  FILLER                PIC X(8)   VALUE SPACE.                    
000412     03  FILLER                PIC X(26)                                  
000413               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
000414     03  FILLER                PIC X(111)  VALUE SPACE.                   
000415                                                                          
000416                                                                          
000417     EJECT                                                                
000418                                                                          
000419 01  W001R2-DELRUBRIK-3-61.                                               
000420     03  FILLER                PIC X(1)   VALUE '0'.                      
000421     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000422     03  FILLER                PIC X(7)   VALUE  SPACE.                   
000423     03  FILLER                PIC X(3)   VALUE  SPACE.                   
000424     03  FILLER                PIC X(31)                                  
000425               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
000426     03  FILLER                PIC X(31)                                  
000427               VALUE '           WARRANTY  WARRANTY  '.                   
000428     03  FILLER                PIC X(3)   VALUE SPACE.                    
000429                                                                          
000430     EJECT                                                                
000431                                                                          
000432*                                                                         
000433*** USA RUBRIKER ****                                                     
000434 01  W001R2-DELRUBRIK-1-41.                                               
000435*                                                                         
000436     03  W001R2-STYR-41        PIC X(1)   VALUE '0'.                      
000437     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000438     03  FILLER                PIC X(7)   VALUE 'DC '.                    
000439     03  FILLER                PIC X(3)   VALUE '   '.                    
000440     03  FILLER                PIC X(26)                                  
000441               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
000442     03  FILLER                PIC X(8)   VALUE SPACE.                    
000443     03  FILLER                PIC X(26)                                  
000444               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
000445     03  FILLER                PIC X(111)  VALUE SPACE.                   
000446                                                                          
000447                                                                          
000448 01  W001R2-DELRUBRIK-3-41.                                               
000449                                                                          
000450     03  FILLER                PIC X(1)   VALUE '0'.                      
000451     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000452     03  FILLER                PIC X(7)   VALUE  SPACE.                   
000453     03  FILLER                PIC X(3)   VALUE  SPACE.                   
000454     03  FILLER                PIC X(31)                                  
000455               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
000456     03  FILLER                PIC X(31)                                  
000457               VALUE '           WARRANTY  WARRANTY  '.                   
000458     03  FILLER                PIC X(3)   VALUE SPACE.                    
000459                                                                          
000460     EJECT                                                                
000470                                                                          
000480 01  W001R2-DELRUBRIK-1-43.                                               
000481*                                                                         
000482     03  W001R2-STYR-43        PIC X(1)   VALUE '0'.                      
000483     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000484     03  FILLER                PIC X(7)   VALUE 'DC '.                    
000485     03  FILLER                PIC X(6)   VALUE '     '.                  
000486     03  FILLER                PIC X(32)                                  
000487               VALUE 'WORKTIME AVERAGE        QUANTITY'.                  
000488     03  FILLER                PIC X(3)  VALUE SPACE.                     
000489     03  FILLER                PIC X(2)   VALUE SPACE.                    
000490     03  FILLER                PIC X(8)   VALUE SPACE.                    
000491     03  FILLER                PIC X(4)   VALUE SPACE.                    
000492     03  FILLER                PIC X(13)  VALUE SPACE.                    
000493                                                                          
000494     EJECT                                                                
000495                                                                          
000496* FOR WEB DC'S                                                            
000497 01  W001R2-DELRUBRIK-1-DC.                                               
000498*                                                                         
000499     03  FILLER                PIC X(5)   VALUE  SPACE.                   
000500     03  FILLER                PIC X(7)   VALUE 'DC '.                    
000501     03  FILLER                PIC X(3)   VALUE '   '.                    
000502     03  FILLER                PIC X(26)                                  
000503               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
000504     03  FILLER                PIC X(8)   VALUE SPACE.                    
000505     03  FILLER                PIC X(26)                                  
000506               VALUE 'WORKTIME AVERAGE  QUANTITY'.                        
000507     03  FILLER                PIC X(111)  VALUE SPACE.                   
000508                                                                          
000509                                                                          
000510     EJECT                                                                
000511                                                                          
000512 01  W001R2-DELRUBRIK-3-DC.                                               
000513     03  FILLER                PIC X(5)   VALUE  SPACE.                   
000514     03  FILLER                PIC X(7)   VALUE  SPACE.                   
000515     03  FILLER                PIC X(3)   VALUE  SPACE.                   
000516     03  FILLER                PIC X(31)                                  
000517               VALUE '   NONE WARRANTY  NONE WARRANTY'.                   
000518     03  FILLER                PIC X(31)                                  
000519               VALUE '           WARRANTY  WARRANTY  '.                   
000520     03  FILLER                PIC X(3)   VALUE SPACE.                    
000521                                                                          
000522     EJECT                                                                
000523                                                                          
000524*                                                                         
000525 01  W001R2-DELRUBRIK-2.                                                  
000526*                                                                         
000527     03  W001R2-STYR           PIC X(1)   VALUE '0'.                      
000528     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000529     03  FILLER                PIC X(2)   VALUE '11'.                     
000530     03  FILLER                PIC X(18)  VALUE SPACE.                    
000531     03  W001R2-ANTAL          PIC Z(7).99.                               
000532     03  FILLER                PIC X(9)  VALUE SPACE.                     
000533     03  W001R2-ANTAL-W        PIC Z(7).99.                               
000534     03  FILLER                PIC X(9)  VALUE SPACE.                     
000535     03  W001R2-KVRETUR-11     PIC Z(6)9.                                 
000536     03  FILLER                PIC X(9)  VALUE SPACE.                     
000537                                                                          
000538     EJECT                                                                
000539*****  BORN RUBRIKER *****                                                
000540 01  W001R2-DELRUBRIK-2-91.                                               
000541*                                                                         
000542*    03  W001R2-STYR-91        PIC X(1)   VALUE '0'.                      
000543*    03  FILLER                PIC X(4)   VALUE  SPACE.                   
000544     03  FILLER                PIC X(2)   VALUE '91'.                     
000545     03  FILLER                PIC X      VALUE ';'.                      
000546     03  W001R2-ANTAL-91       PIC Z(7).99.                               
000547     03  FILLER                PIC X      VALUE ';'.                      
000548     03  W001R2-WORKDAY-91     PIC Z(6)9.                                 
000549     03  FILLER                PIC X      VALUE ';'.                      
000550     03  W001R2-TOTREPT-91     PIC Z(6)9.                                 
000551     03  FILLER                PIC X      VALUE ';'.                      
000552                                                                          
000553                                                                          
000554     EJECT                                                                
000555*****  JAPAN RUBRIKER *****                                               
000556 01  W001R2-DELRUBRIK-2-61.                                               
000557*                                                                         
000558     03  W001R2-STYR-61        PIC X(1)   VALUE '0'.                      
000559     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000560     03  FILLER                PIC X(2)   VALUE '61'.                     
000561     03  FILLER                PIC X(14)  VALUE SPACE.                    
000562     03  W001R2-ANTAL-61       PIC Z(7).99.                               
000563     03  FILLER                PIC X(8)   VALUE SPACE.                    
000564     03  W001R2-KVRETUR-61     PIC Z(6)9.                                 
000565     03  FILLER                PIC X(9)   VALUE SPACE.                    
000566     03  W001R2-ANTAL-61-W     PIC Z(7).99.                               
000567     03  FILLER                PIC X(3)   VALUE SPACE.                    
000568     03  W001R2-KVRETUR-61-W   PIC Z(6)9.                                 
000569     03  FILLER                PIC X(7)  VALUE SPACE.                     
000570                                                                          
000571                                                                          
000572     EJECT                                                                
000573*****  USA RUBRIKER *****                                                 
000574 01  W001R2-DELRUBRIK-2-41.                                               
000575*                                                                         
000576     03  W001R2-STYR-41        PIC X(1)   VALUE '0'.                      
000577     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000578     03  FILLER                PIC X(2)   VALUE '41'.                     
000579     03  FILLER                PIC X(14)  VALUE SPACE.                    
000580     03  W001R2-ANTAL-41       PIC Z(7).99.                               
000581     03  FILLER                PIC X(8)   VALUE SPACE.                    
000582     03  W001R2-KVRETUR-41     PIC Z(6)9.                                 
000583     03  FILLER                PIC X(9)   VALUE SPACE.                    
000584     03  W001R2-ANTAL-41-W     PIC Z(7).99.                               
000585     03  FILLER                PIC X(3)   VALUE SPACE.                    
000586     03  W001R2-KVRETUR-41-W   PIC Z(6)9.                                 
000587     03  FILLER                PIC X(7)  VALUE SPACE.                     
000588                                                                          
000589 01  W001R2-DELRUBRIK-2-43.                                               
000590*                                                                         
000600     03  W001R2-STYR-43        PIC X(1)   VALUE '0'.                      
000601     03  FILLER                PIC X(4)   VALUE  SPACE.                   
000602     03  FILLER                PIC X(2)   VALUE '43'.                     
000603     03  FILLER                PIC X(17)  VALUE SPACE.                    
000604     03  W001R2-ANTAL-43       PIC Z(7).99.                               
000605     03  FILLER                PIC X(9)   VALUE SPACE.                    
000606     03  W001R2-KVRETUR-43     PIC Z(6)9.                                 
000607     03  FILLER                PIC X(2)   VALUE SPACE.                    
000608     03  FILLER                PIC X(8)   VALUE SPACE.                    
000609     03  FILLER                PIC X(3)   VALUE SPACE.                    
000610     03  FILLER                PIC X(13)  VALUE SPACE.                    
000611     EJECT                                                                
000612                                                                          
000613***** WEB DC RUBRIKER *****                                               
000614 01  W001R2-DELRUBRIK-2-DC.                                               
000615*                                                                         
000616     03  FILLER                PIC X(5)   VALUE SPACE.                    
000617     03  W001R2-IDDC           PIC X(2)   VALUE SPACE.                    
000618     03  FILLER                PIC X(14)  VALUE SPACE.                    
000619     03  W001R2-ANTAL-DC       PIC Z(7).99.                               
000620     03  FILLER                PIC X(8)   VALUE SPACE.                    
000621     03  W001R2-KVRETUR-DC     PIC Z(6)9.                                 
000622     03  FILLER                PIC X(9)   VALUE SPACE.                    
000623     03  W001R2-ANTAL-DC-W     PIC Z(7).99.                               
000624     03  FILLER                PIC X(3)   VALUE SPACE.                    
000625     03  W001R2-KVRETUR-DC-W   PIC Z(6)9.                                 
000626     03  FILLER                PIC X(7)   VALUE SPACE.                    
000627                                                                          
000628     EJECT                                                                
000629                                                                          
000630 01  DAP-CONTROL-REC1.                                                    
000631     03  FILLER                PIC X(15)  VALUE ' ¤DAPW37254-001'.        
000632 01  DAP-CONTROL-REC2.                                                    
000633     03  FILLER                PIC X(05)  VALUE ' ¤DAP'.                  
000634     03  DAP-CONTROL-IDDC      PIC X(02)  VALUE SPACE.                    
000635     EJECT                                                                
000636                                                                          
000637 PROCEDURE DIVISION.                                                      
000638 MAIN SECTION.                                                            
000639     SKIP2                                                                
000640                                                                          
000641     PERFORM A-INIT                                                       
000642     PERFORM S01-LAES-W3724D                                              
000643     PERFORM UNTIL END-OF-W3724D                                          
000644       PERFORM B-BEARBETA                                                 
000645       PERFORM S01-LAES-W3724D                                            
000646     END-PERFORM                                                          
000647                                                                          
000648     PERFORM C-BERAKNA                                                    
000649                                                                          
000650     PERFORM S08-SKRIV-UT-RAD-WEBDC                                       
000651                                                                          
000652     PERFORM Z-FINIT                                                      
000653                                                                          
000654     MOVE ZERO TO RETURN-CODE                                             
000655     GOBACK                                                               
000656     .                                                                    
000657     EJECT                                                                
000658 A-INIT SECTION.                                                          
000659                                                                          
000660*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
000661                                                                          
000662     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000663                                                                          
000664     MOVE   D-AAR            TO  DAGENS-DATUM-AAR                         
000665     MOVE   D-MAANAD         TO  DAGENS-DATUM-MAANAD                      
000666     MOVE   D-DAG            TO  DAGENS-DATUM-DAG                         
000667                                                                          
000668     IF DAGENS-DATUM-AAR > 50                                             
000669        MOVE 19              TO DATUM-SEKEL                               
000670                                DATUM-GB-SEKEL                            
000671                                DATUM-US-SEKEL                            
000672     ELSE                                                                 
000673        MOVE 20              TO DATUM-SEKEL                               
000674                                DATUM-GB-SEKEL                            
000675                                DATUM-US-SEKEL                            
000676     END-IF                                                               
000677                                                                          
000678     MOVE DAGENS-DATUM-AAR    TO DATUM-AAR                                
000679                                 DATUM-GB-AAR                             
000680                                 DATUM-US-AAR                             
000681     MOVE DAGENS-DATUM-MAANAD TO DATUM-MAANAD                             
000682                                 DATUM-GB-MAANAD                          
000683                                 DATUM-US-MAANAD                          
000684     MOVE DAGENS-DATUM-DAG    TO DATUM-DAG                                
000685                                 DATUM-GB-DAG                             
000686                                 DATUM-US-DAG                             
000687                                                                          
000688     MOVE WS-DATUM            TO W001R1-DAGENS-DATUM                      
000689     MOVE WS-DATUM-GB         TO W001R1-DAGENS-DATUM91                    
000690     MOVE WS-DATUM-US         TO W001R1-DAGENS-DATUM41                    
000700                                 W001R1-DAGENS-DATUM43                    
000701                                 W001R1-DAGENS-DATUM61                    
000702                                 W001R1-DAGENS-DATUMDC                    
000703                                                                          
000704     OPEN INPUT  W3724D                                                   
000705                                                                          
000706     OPEN OUTPUT LISTA                                                    
000707                 LISTB                                                    
000708                 LISTC                                                    
000709                 LISTE                                                    
000710                 LISTF                                                    
000711                 LISTG                                                    
000712                                                                          
000713***** RUBRIKRAD 1 ******                                                  
000714                                                                          
000715     MOVE W001R1-RUBRIK-S     TO UT-RAD                                   
000716     PERFORM S02-SKRIV-UT-RAD                                             
000717                                                                          
000718     MOVE W001R1-RUBRIK-S91   TO UT-RAD-91                                
000719     PERFORM S03-SKRIV-UT-RAD-91                                          
000720                                                                          
000721     MOVE W001R1-RUBRIK-S41   TO UT-RAD-41                                
000722     PERFORM S04-SKRIV-UT-RAD-41                                          
000723                                                                          
000724     MOVE W001R1-RUBRIK-S43   TO UT-RAD-43                                
000725     PERFORM S06-SKRIV-UT-RAD-43                                          
000726                                                                          
000727     MOVE W001R1-RUBRIK-S61   TO UT-RAD-61                                
000728     PERFORM S07-SKRIV-UT-RAD-61                                          
000729                                                                          
000730***** RUBRIKRAD 2 ******                                                  
000731                                                                          
000732     MOVE W001R2-DELRUBRIK-1  TO UT-RAD                                   
000733     PERFORM S02-SKRIV-UT-RAD                                             
000734                                                                          
000735     MOVE W001R2-DELRUBRIK-3  TO UT-RAD                                   
000736     PERFORM S02-SKRIV-UT-RAD                                             
000737                                                                          
000738     MOVE W001R2-DELRUBRIK-1-91  TO UT-RAD-91                             
000739     PERFORM S03-SKRIV-UT-RAD-91                                          
000740                                                                          
000741     MOVE W001R2-DELRUBRIK-1-41  TO UT-RAD-41                             
000742     PERFORM S04-SKRIV-UT-RAD-41                                          
000743                                                                          
000744     MOVE W001R2-DELRUBRIK-3-41  TO UT-RAD-41                             
000745     PERFORM S04-SKRIV-UT-RAD-41                                          
000746                                                                          
000747     MOVE W001R2-DELRUBRIK-1-43  TO UT-RAD-43                             
000748     PERFORM S06-SKRIV-UT-RAD-43                                          
000749                                                                          
000750     MOVE W001R2-DELRUBRIK-1-61  TO UT-RAD-61                             
000751     PERFORM S07-SKRIV-UT-RAD-61                                          
000752                                                                          
000753     MOVE W001R2-DELRUBRIK-3-61  TO UT-RAD-61                             
000754     PERFORM S07-SKRIV-UT-RAD-61                                          
000755                                                                          
000756     SKIP2                                                                
000757     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000758     .                                                                    
000759     EJECT                                                                
000760 B-BEARBETA SECTION.                                                      
000761                                                                          
000762     MOVE IN-IDDC             TO WS-IDDC                                  
000763                                 WBDC-IDDC                                
000764                                                                          
000765     CALL WL10WBDC            USING WBDC-AREA                             
000766                                                                          
000767     IF WBDC-FLWEBDC = 'J'                                                
000768       PERFORM BA-PROCESS-WEBDC                                           
000769     ELSE                                                                 
000770       EVALUATE TRUE                                                      
000780         WHEN CDC-SE                                                      
000781           IF IN-FLBYTGAR = 'J'                                           
000782             ADD +1            TO W-ANTALPOSTER-W                         
000783             ADD IN-KVARBDAG   TO W-KVARBDAG-W                            
000784             ADD IN-KVRETUR    TO W-KVRETUR11                             
000785           ELSE                                                           
000786             ADD +1            TO W-ANTALPOSTER                           
000787             ADD IN-KVARBDAG   TO W-KVARBDAG                              
000788             ADD IN-KVRETUR    TO W-KVRETUR11                             
000789           END-IF                                                         
000790         WHEN SDC-NL-ET                                                   
000791           ADD +1            TO W-ANTALPOSTER2                            
000792           ADD IN-KVARBDAG   TO W-KVARBDAG2                               
000793           ADD IN-KVRETUR    TO W-KVRETUR91                               
000794         WHEN NDC-US-RU                                                   
000795           IF IN-FLBYTGAR = 'J'                                           
000796              ADD +1            TO W-ANTALPOSTER41-W                      
000797              ADD IN-KVARBDAG   TO W-KVARBDAG41-W                         
000798              ADD IN-KVRETUR    TO W-KVRETUR41-W                          
000799           ELSE                                                           
000800              ADD +1            TO W-ANTALPOSTER41                        
000801              ADD IN-KVARBDAG   TO W-KVARBDAG41                           
000802              ADD IN-KVRETUR    TO W-KVRETUR41                            
000803           END-IF                                                         
000804         WHEN NDC-US-LA                                                   
000805           ADD +1            TO W-ANTALPOSTER43                           
000806           ADD IN-KVARBDAG   TO W-KVARBDAG43                              
000807           ADD IN-KVRETUR    TO W-KVRETUR43                               
000808         WHEN NDC-JP                                                      
000809           IF IN-FLBYTGAR = 'J'                                           
000810              ADD +1            TO W-ANTALPOSTER61-W                      
000811              ADD IN-KVARBDAG   TO W-KVARBDAG61-W                         
000812              ADD IN-KVRETUR    TO W-KVRETUR61-W                          
000813           ELSE                                                           
000814              ADD +1            TO W-ANTALPOSTER61                        
000815              ADD IN-KVARBDAG   TO W-KVARBDAG61                           
000816              ADD IN-KVRETUR    TO W-KVRETUR61                            
000817           END-IF                                                         
000818       END-EVALUATE                                                       
000819     END-IF                                                               
000820                                                                          
000821     .                                                                    
000822     EJECT                                                                
000823                                                                          
000824 BA-PROCESS-WEBDC SECTION.                                                
000825                                                                          
000826     IF IN-IDDC = WS-SPAR-IDDC                                            
000827       IF IN-FLBYTGAR = 'J'                                               
000828         ADD +1                  TO W-ANTALPOSTERDC-W                     
000829         ADD IN-KVARBDAG         TO W-KVARBDAGDC-W                        
000830         ADD IN-KVRETUR          TO W-KVRETURDC-W                         
000831       ELSE                                                               
000832         ADD +1                  TO W-ANTALPOSTERDC                       
000833         ADD IN-KVARBDAG         TO W-KVARBDAGDC                          
000834         ADD IN-KVRETUR          TO W-KVRETURDC                           
000835       END-IF                                                             
000836     ELSE                                                                 
000837       PERFORM S08-SKRIV-UT-RAD-WEBDC                                     
000838       MOVE IN-IDDC              TO WS-SPAR-IDDC                          
000839       MOVE ZERO                 TO W-ANTALPOSTERDC-W                     
000840                                    W-KVARBDAGDC-W                        
000841                                    W-KVRETURDC-W                         
000842                                    W-ANTALPOSTERDC                       
000843                                    W-KVARBDAGDC                          
000844                                    W-KVRETURDC                           
000845                                                                          
000846       IF IN-FLBYTGAR = 'J'                                               
000847         ADD +1                  TO W-ANTALPOSTERDC-W                     
000848         ADD IN-KVARBDAG         TO W-KVARBDAGDC-W                        
000849         ADD IN-KVRETUR          TO W-KVRETURDC-W                         
000850       ELSE                                                               
000851         ADD +1                  TO W-ANTALPOSTERDC                       
000852         ADD IN-KVARBDAG         TO W-KVARBDAGDC                          
000853         ADD IN-KVRETUR          TO W-KVRETURDC                           
000854       END-IF                                                             
000855     END-IF                                                               
000856                                                                          
000857     .                                                                    
000858     EJECT                                                                
000859                                                                          
000860 C-BERAKNA SECTION.                                                       
000861                                                                          
000862     IF W-ANTALPOSTER > ZERO                                              
000863        COMPUTE W-ANTALSUMMA  =  W-KVARBDAG /  W-ANTALPOSTER              
000864     END-IF                                                               
000865     IF W-ANTALPOSTER-W > ZERO                                            
000866        COMPUTE W-ANTALSUMMA-W = W-KVARBDAG-W / W-ANTALPOSTER-W           
000867     END-IF                                                               
000868     IF W-ANTALPOSTER2 > ZERO                                             
000869        COMPUTE W-ANTALSUMMA2 =  W-KVARBDAG2 / W-ANTALPOSTER2             
000870     END-IF                                                               
000871     IF W-ANTALPOSTER41 > ZERO                                            
000872        COMPUTE W-ANTALSUMMA41 =  W-KVARBDAG41 / W-ANTALPOSTER41          
000873     END-IF                                                               
000874     IF W-ANTALPOSTER41-W > ZERO                                          
000875        COMPUTE W-ANTALSUMMA41-W =                                        
000876                        W-KVARBDAG41-W / W-ANTALPOSTER41-W                
000877     END-IF                                                               
000878     IF W-ANTALPOSTER43 > ZERO                                            
000879        COMPUTE W-ANTALSUMMA43 =  W-KVARBDAG43 / W-ANTALPOSTER43          
000880     END-IF                                                               
000890     IF W-ANTALPOSTER61 > ZERO                                            
000891        COMPUTE W-ANTALSUMMA61 =                                          
000892                        W-KVARBDAG61 / W-ANTALPOSTER61                    
000893     END-IF                                                               
000894     IF W-ANTALPOSTER61-W > ZERO                                          
000895        COMPUTE W-ANTALSUMMA61-W =                                        
000896                        W-KVARBDAG61-W / W-ANTALPOSTER61-W                
000897     END-IF                                                               
000898                                                                          
000899     MOVE W-ANTALSUMMA       TO W001R2-ANTAL                              
000900     MOVE W-ANTALSUMMA-W     TO W001R2-ANTAL-W                            
000901     MOVE W-ANTALSUMMA2      TO W001R2-ANTAL-91                           
000902     MOVE W-ANTALSUMMA41     TO W001R2-ANTAL-41                           
000903     MOVE W-ANTALSUMMA41-W   TO W001R2-ANTAL-41-W                         
000904     MOVE W-ANTALSUMMA43     TO W001R2-ANTAL-43                           
000905     MOVE W-ANTALSUMMA61     TO W001R2-ANTAL-61                           
000906     MOVE W-ANTALSUMMA61-W   TO W001R2-ANTAL-61-W                         
000907     MOVE W-KVRETUR11        TO W001R2-KVRETUR-11                         
000908     MOVE W-KVARBDAG2        TO W001R2-WORKDAY-91                         
000909     MOVE W-ANTALPOSTER2     TO W001R2-TOTREPT-91                         
000910     MOVE W-KVRETUR41        TO W001R2-KVRETUR-41                         
000911     MOVE W-KVRETUR41-W      TO W001R2-KVRETUR-41-W                       
000912     MOVE W-KVRETUR43        TO W001R2-KVRETUR-43                         
000913     MOVE W-KVRETUR61        TO W001R2-KVRETUR-61                         
000914     MOVE W-KVRETUR61-W      TO W001R2-KVRETUR-61-W                       
000915                                                                          
000916                                                                          
000917***** RUBRIKRAD 3 ******                                                  
000918                                                                          
000919     MOVE W001R2-DELRUBRIK-2 TO UT-RAD                                    
000920     PERFORM S02-SKRIV-UT-RAD                                             
000921                                                                          
000922     MOVE W001R2-DELRUBRIK-2-91 TO UT-RAD-91                              
000923     PERFORM S03-SKRIV-UT-RAD-91                                          
000924                                                                          
000925     MOVE W001R2-DELRUBRIK-2-41 TO UT-RAD-41                              
000926     PERFORM S04-SKRIV-UT-RAD-41                                          
000927                                                                          
000928     MOVE W001R2-DELRUBRIK-2-43 TO UT-RAD-43                              
000929     PERFORM S06-SKRIV-UT-RAD-43                                          
000930                                                                          
000931     MOVE W001R2-DELRUBRIK-2-61 TO UT-RAD-61                              
000932     PERFORM S07-SKRIV-UT-RAD-61                                          
000933                                                                          
000934     .                                                                    
000935     EJECT                                                                
000936 Z-FINIT SECTION.                                                         
000937     CLOSE W3724D                                                         
000938           LISTA                                                          
000939           LISTB                                                          
000940           LISTC                                                          
000941           LISTE                                                          
000942           LISTF                                                          
000943           LISTG                                                          
000944     SKIP2                                                                
000945     MOVE 'S' TO POSTSUM-OPKOD                                            
000946     CALL POSTSUM USING POSTSUM-PARM                                      
000947     .                                                                    
000948     EJECT                                                                
000949 S01-LAES-W3724D  SECTION.                                                
000950     READ W3724D INTO IN-AREA                                             
000951     AT END                                                               
000952        MOVE HIGH-VALUE TO IN-AREA                                        
000953        SET END-OF-W3724D TO TRUE                                         
000954                                                                          
000955     NOT AT END                                                           
000956        MOVE 'W3724D' TO POSTSUM-FDNAMN                                   
000957        MOVE 'W37254D1' TO POSTSUM-DDNAMN2                                
000958        MOVE ' UT '    TO POSTSUM-TRANSTYP                                
000959        CALL POSTSUM USING POSTSUM-PARM                                   
000960     END-READ                                                             
000961     .                                                                    
000962     EJECT                                                                
000963 S02-SKRIV-UT-RAD SECTION.                                                
000964     SKIP2                                                                
000965                                                                          
000966     WRITE LISTAS FROM UT-RAD                                             
000967                                                                          
000968     .                                                                    
000969     EJECT                                                                
000970 S03-SKRIV-UT-RAD-91 SECTION.                                             
000971     SKIP2                                                                
000972                                                                          
000973     WRITE LISTBS FROM UT-RAD-91                                          
000974                                                                          
000975     .                                                                    
000976     EJECT                                                                
000977 S04-SKRIV-UT-RAD-41 SECTION.                                             
000978     SKIP2                                                                
000979                                                                          
000980     WRITE LISTCS FROM UT-RAD-41                                          
000981                                                                          
000982     .                                                                    
000983     EJECT                                                                
000984 S06-SKRIV-UT-RAD-43 SECTION.                                             
000985     SKIP2                                                                
000986                                                                          
000987     WRITE LISTES FROM UT-RAD-43                                          
000988                                                                          
000989     .                                                                    
000990     EJECT                                                                
001000 S07-SKRIV-UT-RAD-61 SECTION.                                             
001001     SKIP2                                                                
001002                                                                          
001003     WRITE LISTFS FROM UT-RAD-61                                          
001004                                                                          
001005     .                                                                    
001006     EJECT                                                                
001007 S08-SKRIV-UT-RAD-WEBDC SECTION.                                          
001008                                                                          
001009     MOVE WS-SPAR-IDDC           TO WBDC-IDDC                             
001010                                    WS-IDDC                               
001011                                                                          
001012     CALL WL10WBDC            USING WBDC-AREA                             
001013                                                                          
001014     IF WBDC-FLWEBDC = 'J'                                                
001015                                                                          
001016*      DAP CONTROL RECORDS                                                
001017       MOVE DAP-CONTROL-REC1     TO UT-RAD-DC                             
001018       WRITE LISTGS            FROM UT-RAD-DC                             
001019                                                                          
001020       MOVE WS-SPAR-IDDC         TO DAP-CONTROL-IDDC                      
001030       MOVE DAP-CONTROL-REC2     TO UT-RAD-DC                             
001031       WRITE LISTGS            FROM UT-RAD-DC                             
001032                                                                          
001033*      REPORT HEADER 1                                                    
001034       MOVE W001R1-RUBRIK-DC     TO UT-RAD-DC                             
001035       WRITE LISTGS            FROM UT-RAD-DC                             
001036                                                                          
001037*      REPORT HEADER 2                                                    
001038       MOVE W001R2-DELRUBRIK-1-DC                                         
001039                                 TO UT-RAD-DC                             
001040       WRITE LISTGS            FROM UT-RAD-DC                             
001041                                                                          
001042*      REPORT HEADER 3                                                    
001043       MOVE W001R2-DELRUBRIK-3-DC                                         
001044                                 TO UT-RAD-DC                             
001045       WRITE LISTGS            FROM UT-RAD-DC                             
001046                                                                          
001047*      REPORT DATA                                                        
001048       IF W-ANTALPOSTERDC > ZERO                                          
001049         COMPUTE W-ANTALSUMMADC =                                         
001050                        W-KVARBDAGDC / W-ANTALPOSTERDC                    
001051       END-IF                                                             
001052       IF W-ANTALPOSTERDC-W > ZERO                                        
001053         COMPUTE W-ANTALSUMMADC-W =                                       
001054                        W-KVARBDAGDC-W / W-ANTALPOSTERDC-W                
001055       END-IF                                                             
001056                                                                          
001057       MOVE WS-SPAR-IDDC         TO W001R2-IDDC                           
001058       MOVE W-ANTALSUMMADC       TO W001R2-ANTAL-DC                       
001059       MOVE W-ANTALSUMMADC-W     TO W001R2-ANTAL-DC-W                     
001060       MOVE W-KVRETURDC          TO W001R2-KVRETUR-DC                     
001061       MOVE W-KVRETURDC-W        TO W001R2-KVRETUR-DC-W                   
001062                                                                          
001063       MOVE W001R2-DELRUBRIK-2-DC                                         
001064                                 TO UT-RAD-DC                             
001065       WRITE LISTGS            FROM UT-RAD-DC                             
001066     END-IF                                                               
001067     .                                                                    
001068     EJECT                                                                
001069 S99-ABEND SECTION.                                                       
001070                                                                          
001071     SKIP2                                                                
001072     MOVE 'S' TO POSTSUM-OPKOD                                            
001073     CALL POSTSUM USING POSTSUM-PARM                                      
001074     CALL ABEND USING RKOD-ABEND                                          
001075     .                                                                    
