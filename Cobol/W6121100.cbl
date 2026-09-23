000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W6121100.                                                
000003 AUTHOR.         TOMMIE JIVARP.                                           
000004 DATE-WRITTEN.   97/12/11.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        PROGRAMMET SKRIVER EN UTFIL MED LAGERPLATSINFO IFRÅN             
000009*        WDJ8 OCH FREKVENSKOD - PERIODBEHOV IFRÅN WDR2                    
000010*                                                                         
000011*        PROGRAMMET LÄSER      WLLOCA (WDJ8)                              
000012*                              WL6313 (WDR2)                              
000013*                              WDB6                                       
000014*                                                                         
000015*    ABENDKODER:                                                          
000016*        U0016 -  . . . .                                                 
000017*        U1000 -  . . . .                                                 
000018*                                                                         
000019                                                                          
000020     SKIP3                                                                
000021 ENVIRONMENT DIVISION.                                                    
000022     SKIP2                                                                
000023 INPUT-OUTPUT SECTION.                                                    
000024                                                                          
000025 FILE-CONTROL.                                                            
000026     SKIP2                                                                
000027*          --- UTFIL .........                                            
000028     SELECT W61211                     ASSIGN TO W61211D1.                
000029     EJECT                                                                
000030 DATA DIVISION.                                                           
000031     SKIP2                                                                
000032 FILE SECTION.                                                            
000033     SKIP3                                                                
000034 FD  W61211                                                               
000035     RECORDING       F                                                    
000036     BLOCK CONTAINS  0.                                                   
000037                                                                          
000038*01  POST -COPY W61211 -PRE  UT1-  -L.                                    
000039     EJECT                                                                
000040 WORKING-STORAGE SECTION.                                                 
000041                                                                          
000042                                                                          
000043*    -- CHECKED BY WY2000                                                 
000044 77  IDPGM                       PIC X(8)    VALUE 'W6121100'.            
000045 77  JA                          PIC X       VALUE 'J'.                   
000046 77  NEJ                         PIC X       VALUE 'N'.                   
000047     EJECT                                                                
000048 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000049 01  FILLER REDEFINES DAGENS-DATUM.                                       
000050     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000051     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000052     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000053     EJECT                                                                
000054 01  DYNAMISKA-SUBPROGRAM.                                                
000055*                                                                         
000056     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000057     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000059     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000060     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000061     SKIP2                                                                
000062                                                                          
000063*    --- MINNESTABELL FÖR DATA IFRÅN WDR2                                 
000064 01  FREKVENSTABELL.                                                      
000065    03 IDDC-TAB OCCURS 100.                                               
000066       05  FREKV-IDDC       PIC X(2)         VALUE SPACE.                 
000067       05 FREKV-TAB OCCURS 30.                                            
000068          07  TAB-KDFREQ    PIC X(2)         VALUE SPACE.                 
000069          07  TAB-KVPB-FOM  PIC S9(6)V9(1) COMP-3                         
000070                                             VALUE ZERO.                  
000071          07  TAB-KVPB-TOM  PIC S9(6)V9(1) COMP-3                         
000072                                             VALUE ZERO.                  
000073          07  TAB-RELOCFAC  PIC 9V9(2)       VALUE ZERO.                  
000074                                                                          
000075*    --- DC/ FREKVENSINDEX                                                
000076 01  DC-IX                       PIC S9(9)   VALUE ZERO.                  
000078 01  DC-IX-MAX                   PIC S9(9)   VALUE 100.                   
000079 01  FREKV-IX                    PIC S9(9)   VALUE ZERO.                  
000080 01  FREKV-IX-MAX                PIC S9(9)   VALUE 30.                    
000081 01  SW-IDDC                     PIC X       VALUE SPACE.                 
000082                                                                          
000083*    --- PARAMETRAR TILL ABEND                                            
000084                                                                          
000085 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000086 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000087 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000088     SKIP2                                                                
000089 01  FELTEXT.                                                             
000090     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000091     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000092     EJECT                                                                
000093*    --- PARAMETRAR TILL POSTSUM                                          
000094*                                                                         
000095*01  -COPY W0005   -PRE  POSTSUM-                                         
000096     EJECT                                                                
000097 01  UT1-AREA-START              PIC X(24)   VALUE                        
000098                                 'UT1-AREA-START  '.                      
000099     SKIP2                                                                
000100                                                                          
000101*01  AREA -COPY W61211     -PRE UT1-                                      
000102     EJECT                                                                
000103*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000104*                                                                         
000105     EJECT                                                                
000106 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000107     SKIP3                                                                
000108                                                                          
000109 01  NYCKLAR-TILL-DLI.                                                    
000110     03  W-WDGXKEY-6313-X.                                                
000111         05  W-IDHTYP            PIC X(4)    VALUE '6313'.                
000112         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000113         05  W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
000114                                                                          
000115     03  W-WDGXKEY-6314-X.                                                
000116         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
000117                                                                          
000118     03  W-WDJ801KY-X.                                                    
000119         05  W-WDJ801KY          PIC X(11)   VALUE SPACE.                 
000120                                                                          
000121                                                                          
000122*    --- STATUS-KOD FRÅN IMS                                              
000123 01  STATUS-WS                   PIC XX.                                  
000124     88  SEGMENT-FINNS                       VALUE '  '.                  
000125     88  SEGMENT-SAKNAS                      VALUE 'GB'                   
000126                                                   'GE'.                  
000127     88  BASEN-SLUT                          VALUE 'GB'.                  
000128     SKIP2                                                                
000129 01  GODK-STATUSKODER.                                                    
000130     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000131     SKIP3                                                                
000132 01  SSA1                        PIC X(64).                               
000133 01  SSA2                        PIC X(64).                               
000134     EJECT                                                                
000135*    --- IMS FUNKTIONSKODER                                               
000136*01  -COPY W0003                                                          
000137     EJECT                                                                
000138*    ---  DLI INPUT-OUTPUT AREA                                           
000139                                                                          
000140 01  FILLER         PIC X(30) VALUE 'WL631301-AREA'.                      
000141 01  WL631301-AREA.                                                       
000142*    03  -COPY WDGX6313                                                   
000143 01  FILLER         PIC X(23) VALUE 'WL631311-AREA'.                      
000144 01  WL631311-AREA.                                                       
000145*    03  -COPY WDGX6314                                                   
000146 01  FILLER         PIC X(34) VALUE 'DLI-IO-WLLOCA'.                      
000147 01  DLI-IO-WLLOCA.                                                       
000148*    03  -COPY WDJ801                                                     
000149                                                                          
000150 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000151 01   DLI-IO-AREA-B601.                                                   
000152*     03  -COPY WDB601                                                    
000153     EJECT                                                                
000154                                                                          
000155 LINKAGE SECTION.                                                         
000156                                                                          
000157*01  -COPY W0008  -PRE LOCA-                                              
000158     05  FILLER                  PIC X.                                   
000159     EJECT                                                                
000160*01  -COPY W0008  -PRE 6313-                                              
000161     05  FILLER                  PIC X.                                   
000162     EJECT                                                                
000163*01  -COPY W0008  -PRE WDB6-                                              
000164     05  FILLER                  PIC X.                                   
000165     EJECT                                                                
000166                                                                          
000167 PROCEDURE DIVISION  USING LOCA-PCB 6313-PCB WDB6-PCB.                    
000168 MAIN SECTION.                                                            
000169     ENTRY 'DLITCBL' USING LOCA-PCB 6313-PCB WDB6-PCB.                    
000170                                                                          
000171                                                                          
000172     PERFORM A-INIT                                                       
000173                                                                          
000174     PERFORM B-LAES-FLYTTA-TILL-FREKV-TAB                                 
000176     PERFORM IMS-GN-LOCA                                                  
000177     PERFORM UNTIL SEGMENT-SAKNAS                                         
000178       EVALUATE LOCA-SEG-NAME-FB                                          
000179         WHEN 'WDJ801'                                                    
000180           PERFORM C-HAEMTA-FREKVENSTABELL-INFO                           
000181           IF SW-IDDC = 'J'                                               
000182           PERFORM D-FLYTTA-TILL-UT1-AREA                                 
000183           PERFORM S11-SKRIV-W61211                                       
000184           END-IF                                                         
000185       END-EVALUATE                                                       
000186       PERFORM IMS-GN-LOCA                                                
000187     END-PERFORM                                                          
000188     PERFORM Z-FINIT                                                      
000189                                                                          
000190     MOVE ZERO TO RETURN-CODE                                             
000191     GOBACK                                                               
000192     .                                                                    
000193     EJECT                                                                
000194                                                                          
000195 A-INIT SECTION.                                                          
000196                                                                          
000197     OPEN OUTPUT W61211                                                   
000198                                                                          
000199     ACCEPT DAGENS-DATUM  FROM DATE                                       
000200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000201                                                                          
000202     INITIALIZE FREKVENSTABELL                                            
000203     .                                                                    
000204     EJECT                                                                
000205                                                                          
000206 B-LAES-FLYTTA-TILL-FREKV-TAB SECTION.                                    
000207                                                                          
000208     MOVE +1 TO DC-IX                                                     
000209     MOVE +1 TO FREKV-IX                                                  
000210     PERFORM IMS-GN-WDB601                                                
000211     PERFORM UNTIL BASEN-SLUT OR DC-IX > DC-IX-MAX                        
000212       IF NOT DCS-DDC                                                     
000213        IF DCS-FLWEBDC = 'N'                                              
000214         MOVE DCS-IDDC TO FREKV-IDDC(DC-IX)                               
000215                          W-IDDC                                          
000216         PERFORM IMS-GU-WL631301                                          
000217         IF SEGMENT-FINNS                                                 
000218           PERFORM IMS-GNP-WL631311                                       
000219           PERFORM UNTIL SEGMENT-SAKNAS OR FREKV-IX > FREKV-IX-MAX        
000220             MOVE 6314-KDFREQ   TO TAB-KDFREQ   (DC-IX, FREKV-IX)         
000221             MOVE 6314-KVPB-FOM TO TAB-KVPB-FOM (DC-IX, FREKV-IX)         
000222             MOVE 6314-KVPB-TOM TO TAB-KVPB-TOM (DC-IX, FREKV-IX)         
000223             MOVE 6314-RELOCFAC TO TAB-RELOCFAC (DC-IX, FREKV-IX)         
000224             ADD +1 TO FREKV-IX                                           
000225             PERFORM IMS-GNP-WL631311                                     
000226           END-PERFORM                                                    
000227           MOVE +1 TO FREKV-IX                                            
000228         END-IF                                                           
000229         ADD +1 TO DC-IX                                                  
000230        END-IF                                                            
000231       END-IF                                                             
000232       PERFORM IMS-GN-WDB601                                              
000233     END-PERFORM                                                          
000234     .                                                                    
000235     EJECT                                                                
000236                                                                          
000237 C-HAEMTA-FREKVENSTABELL-INFO SECTION.                                    
000238                                                                          
000239     MOVE 'N' TO SW-IDDC                                                  
000240     MOVE +1 TO DC-IX                                                     
000241     MOVE +1 TO FREKV-IX                                                  
000242     PERFORM UNTIL DC-IX > DC-IX-MAX OR FREKV-IDDC(DC-IX) = SPACE         
000243       IF FREKV-IDDC (DC-IX) = LOC-IDDC                                   
000244         PERFORM UNTIL FREKV-IX > FREKV-IX-MAX                            
000245           IF TAB-KDFREQ (DC-IX, FREKV-IX) = LOC-KDFREQ                   
000246             MOVE TAB-KVPB-FOM (DC-IX, FREKV-IX) TO UT1-KVPB-FOM          
000247             MOVE TAB-KVPB-TOM (DC-IX, FREKV-IX) TO UT1-KVPB-TOM          
000248             MOVE TAB-RELOCFAC (DC-IX, FREKV-IX) TO UT1-RELOCFAC          
000249             MOVE 'J' TO SW-IDDC                                          
000250             ADD +1 TO FREKV-IX                                           
000251           ELSE                                                           
000252             ADD +1 TO FREKV-IX                                           
000253           END-IF                                                         
000254         END-PERFORM                                                      
000255         MOVE +1 TO FREKV-IX                                              
000256       END-IF                                                             
000257       ADD +1 TO DC-IX                                                    
000258     END-PERFORM                                                          
000259     .                                                                    
000260     EJECT                                                                
000261                                                                          
000262 D-FLYTTA-TILL-UT1-AREA SECTION.                                          
000263                                                                          
000264     MOVE LOC-IDDC      TO UT1-IDDC                                       
000265     MOVE LOC-ADLAGOMR  TO UT1-ADLAGOMR                                   
000266     MOVE LOC-ADGANG    TO UT1-ADGANG                                     
000267     MOVE LOC-ADPLATS   TO UT1-ADPLATS                                    
000268     MOVE LOC-KDFREQ    TO UT1-KDFREQ                                     
000269     MOVE LOC-KDSTOR    TO UT1-KDSTOR                                     
000270     .                                                                    
000271     EJECT                                                                
000272                                                                          
000273 Z-FINIT SECTION.                                                         
000274     CLOSE W61211                                                         
000275     SKIP2                                                                
000276     MOVE 'S' TO POSTSUM-OPKOD                                            
000277     CALL POSTSUM USING POSTSUM-PARM                                      
000278     .                                                                    
000279     EJECT                                                                
000280                                                                          
000281 S11-SKRIV-W61211 SECTION.                                                
000282                                                                          
000283     WRITE UT1-POST FROM UT1-AREA                                         
000284                                                                          
000285     MOVE SPACE TO POSTSUM-TRANSTYP                                       
000286     MOVE 'W61211' TO POSTSUM-FDNAMN                                      
000287     MOVE 'W61211D1' TO POSTSUM-DDNAMN2                                   
000288     CALL POSTSUM USING POSTSUM-PARM                                      
000289     .                                                                    
000290     EJECT                                                                
000291                                                                          
000292* --- IMS SEKTIONER ---                                                   
000293     SKIP3                                                                
000294     EJECT                                                                
000295                                                                          
000296 IMS-GU-WL631301 SECTION.                                                 
000297                                                                          
000298     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
000299          DELIMITED BY SIZE INTO SSA1                                     
000300     MOVE '  GE' TO GODK-STATUSKODER                                      
000301     CALL CBLTDLI USING GU 6313-PCB WL631301-AREA SSA1                    
000302     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
000303     PERFORM IMS-STATUSKONTROLL                                           
000304     .                                                                    
000305     SKIP3                                                                
000306                                                                          
000307 IMS-GNP-WL631311 SECTION.                                                
000308                                                                          
000309     MOVE 'WL631311 ' TO SSA1                                             
000310     MOVE '  GE' TO GODK-STATUSKODER                                      
000311     CALL CBLTDLI USING GNP 6313-PCB WL631311-AREA SSA1                   
000312     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
000313     PERFORM IMS-STATUSKONTROLL                                           
000314     .                                                                    
000315     SKIP3                                                                
000316 IMS-GN-LOCA   SECTION.                                                   
000317                                                                          
000318     CALL CBLTDLI USING GN LOCA-PCB DLI-IO-WLLOCA                         
000319     MOVE LOCA-STATUS-CODE TO STATUS-WS                                   
000320     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000321     PERFORM IMS-STATUSKONTROLL                                           
000322     .                                                                    
000323     EJECT                                                                
000324 IMS-GN-WDB601    SECTION.                                                
000325     MOVE 'WDB601  ' TO SSA1                                              
000326     MOVE '  GB' TO GODK-STATUSKODER                                      
000327     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000328     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000329     PERFORM IMS-STATUSKONTROLL                                           
000330     .                                                                    
000331                                                                          
000332 IMS-STATUSKONTROLL SECTION.                                              
000333                                                                          
000334     SET STATUS-IX TO 1                                                   
000335     SEARCH GODK-STATUS                                                   
000336       AT END                                                             
000337         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000338           DELIMITED BY SIZE INTO FELTEXT                                 
000339         DISPLAY FELTEXT                                                  
000340         CALL FELLOG                                                      
000341       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000342         CONTINUE                                                         
000343     END-SEARCH                                                           
000350     .                                                                    
