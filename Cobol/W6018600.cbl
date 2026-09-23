000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W6018600.                                                
000003 AUTHOR.         SANTHOSH KUMAR ANGAMUTHU.                                
000004 DATE-WRITTEN.   21/02/17.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNCTION:                                                            
000008*        SCREEN FOR COO HISTORY AND PREFERENTIAL STATUS                   
000009*                                                                         
000010*        THE PROGRAM READS     WDK6                                       
000011*        THE PROGRAM READS     WDM1                                       
000012*        THE PROGRAM READS     WDD3                                       
000013*        THE PROGRAM READS     WDT5                                       
000014*                                                                         
000015*    INDATA.                                                              
000016*        TRANSACTION: W6T186                                              
000017*        MID:         W6I18601                                            
000018*                                                                         
000019*    OUTDATA.                                                             
000020*        MOD:         W6O18601                                            
000021                                                                          
000022     SKIP3                                                                
000023 ENVIRONMENT DIVISION.                                                    
000024                                                                          
000025 DATA DIVISION.                                                           
000026     EJECT                                                                
000027 WORKING-STORAGE SECTION.                                                 
000028 77  IDPGM                       PIC X(08)   VALUE 'W6018600'.            
000029                                                                          
000030*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
000031 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
000032                                                                          
000033 77  YES                         PIC X       VALUE 'J'.                   
000034 77  NOO                         PIC X       VALUE 'N'.                   
000035                                                                          
000036*    --- INDEX FOR SCROLL LINES                                           
000037 77  INDX                      PIC S9(4)  VALUE +0    COMP SYNC.          
000038 77  INDX-DET                  PIC S9(4)  VALUE +0    COMP SYNC.          
000039 77  MAX-INDX                  PIC S9(4)  VALUE +12   COMP SYNC.          
000040*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
000041                                                                          
000042                                                                          
000043 77  KEYS-SW                     PIC X       VALUE 'J'.                   
000044     88  KEYS-OK                             VALUE 'J'.                   
000045     88  KEYS-WRONG                          VALUE 'N'.                   
000046                                                                          
000047 77  PCOO-SW                     PIC X       VALUE 'N'.                   
000048     88  PCOO-FOUND                          VALUE 'J'.                   
000049     88  PCOO-NOT-FOUND                      VALUE 'N'.                   
000050                                                                          
000051 77  PCOO-FIRST-SW               PIC X       VALUE 'J'.                   
000052     88  FIRST-PCOO                          VALUE 'N'.                   
000053     88  NOT-FIRST-PCOO                      VALUE 'J'.                   
000054                                                                          
000055 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000056     88  OWN-MID                             VALUE '6186'.                
000057     88  GOOD-MID                            VALUE '6181' '6182'          
000058                                                   '6183' '6184'          
000059                                                   '6185' '6186'          
000060                                                   '6187' '6188'          
000061                                                   '6189'.                
000062     88  HELP-MID                            VALUE '0551'.                
000063     EJECT                                                                
000064 77  W-DADATTID-9KOMPL           PIC 9(14)                                
000065                                 VALUE 99999999999999.                    
000066 77  W-DADATTID-9KOMPL-COM       PIC 9(14)                                
000067                                 VALUE 99999999999999.                    
000068 77  TODAYS-DATE             PIC 9(6)    VALUE ZERO.                      
000069 77  WS-BEAVTAL              PIC X(6)    VALUE SPACE.                     
000070 77  WS-COUNT-BEVTAL         PIC 9(2)    VALUE ZERO.                      
000071*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
000072  1  GENERAL-SUBPROGRAMS.                                                 
000073     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000074     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000075     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000076     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000077     EJECT                                                                
000078*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
000079*01 -COPY WMEDAREA                                                        
000080     SKIP3                                                                
000081 01  MESSAGE-CODES.                                                       
000082     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000083     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000084     03  INF-NO-MORE-INFO        PIC X(3)    VALUE '106'.                 
000085     03  PARTNO-MISSING          PIC X(3)    VALUE '017'.                 
000086     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000087     EJECT                                                                
000088*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
000089*                                                                         
000090 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000091     SKIP3                                                                
000092*01 -COPY WMSGINIT                                                        
000093     EJECT                                                                
000094*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
000095*                                                                         
000096 01  SAVE-AREA.                                                           
000097     03  SAVE-IDTRANS           PIC X(4)    VALUE '6186'.                 
000098     03  SAVE-UPDAT-ENTER       PIC S9(14)       COMP-3.                  
000099     03  SAVE-UPDAT-NEXT        PIC S9(14)       COMP-3.                  
000100                                                                          
000101*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
000102*                                                                         
000103 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000104     SKIP3                                                                
000105*01  MID -COPY W6I18601                                                   
000106     EJECT                                                                
000107 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000108     SKIP3                                                                
000109*01  -COPY WMSGAREA                                                       
000110     EJECT                                                                
000111     03  MOD REDEFINES MSG-AREA.                                          
000112*      05  -COPY W6O18601                                                 
000113     EJECT                                                                
000114 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000115     SKIP3                                                                
000116*01  -COPY WMFSAREA                                                       
000117     EJECT                                                                
000118*    --- WORK-AREAS FOR IMS-SECTIONS                                      
000119*                                                                         
000120 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000121     SKIP3                                                                
000122 01  KEYS-FOR-DLI.                                                        
000123*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
000124     03  W-UPDAT-MIN-X.                                                   
000125         05  W-UPDAT-MIN     PIC S9(14).                                  
000126                                                                          
000127     03  W-IDARTNR-X.                                                     
000128         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000129     03  W-IDSKYLT-X.                                                     
000130         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
000131     03  W-IDLEVNR-X.                                                     
000132         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
000133     03  W-BEAVTAL-X.                                                     
000134         05  W-BEAVTAL           PIC X(10)   VALUE SPACE.                 
000135     03  W-BEAVTAL-MIN-X.                                                 
000136         05  W-BEAVTAL-MIN       PIC X(10)   VALUE SPACE.                 
000137     03  W-BEAVTAL-MAX-X.                                                 
000138         05  W-BEAVTAL-MAX       PIC X(10)   VALUE SPACE.                 
000139     03  W-WDM101KY-X.                                                    
000140         05  W-IDARTNR-WDM1      PIC S9(9)   VALUE ZERO COMP-3.           
000141         05  W-IDLEVNR-WDM1      PIC X(5)    VALUE SPACE.                 
000142     03  W-WDT511-KEY-MIN-X.                                              
000143         05  W-DADATTID-9KOMPL-MIN PIC 9(14)                              
000144                                   VALUE 99999999999999.                  
000145                                                                          
000146     03  W-WDT511-KEY-MAX-X.                                              
000147         05  W-DADATTID-9KOMPL-MAX PIC 9(14)                              
000148                                   VALUE 00000000000000.                  
000149     03  W-KDSEGKEY-X.                                                    
000150         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
000151     03  W-WDGXKEY01-X.                                                   
000152         05  W-WDGXKEY01.                                                 
000153             07  W-IDHTYP        PIC X(4)    VALUE '5109'.                
000154             07  W-FILLER        PIC X(26)   VALUE LOW-VALUE.             
000155                                                                          
000156                                                                          
000157                                                                          
000158     SKIP2                                                                
000159*    --- STATUS CODES FROM IMS                                            
000160 01  STATUS-WS                   PIC XX.                                  
000161     88  SEGMENT-FOUND                       VALUE '  '.                  
000162     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
000163     88  SEGMENT-MISSING                     VALUE 'GE'.                  
000164     SKIP2                                                                
000165 01  GOOD-STATUSCODES.                                                    
000166     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000167     SKIP3                                                                
000168 01  SSA1                        PIC X(128).                              
000169 01  SSA2                        PIC X(64).                               
000170     EJECT                                                                
000171*    --- IMS FUNCTION CODES                                               
000172*01  -COPY W0003                                                          
000173     EJECT                                                                
000174*    ---  DLI INPUT-OUTPUT AREA                                           
000175                                                                          
000176 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
000177 01  DLI-IO-WDK611.                                                       
000178*    03  -COPY WDK611                                                     
000179     EJECT                                                                
000180 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
000181 01  DLI-IO-WDK601.                                                       
000182*    03  -COPY WDK601                                                     
000183 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT511'.                      
000184 01  DLI-IO-WDT511.                                                       
000185*    03  -COPY WDT511                                                     
000186     EJECT                                                                
000187 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT501'.                      
000188 01  DLI-IO-WDT501.                                                       
000189*    03  -COPY WDT501  -PRE WDT5-                                         
000190 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDD311'.                     
000191 01  DLI-IO-WDD311.                                                       
000192*    03  -COPY WDD311                                                     
000193 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM101'.                      
000194 01  DLI-IO-WDM101.                                                       
000195*    03  -COPY WDM101                                                     
000196     EJECT                                                                
000197 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM121'.                      
000198 01  DLI-IO-WDM121.                                                       
000199*    03  -COPY WDM121                                                     
000200     EJECT                                                                
000201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM111'.                      
000202 01  DLI-IO-WDM111.                                                       
000203*    03  -COPY WDM111                                                     
000204     EJECT                                                                
000205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
000206 01  DLI-IO-WDGX01.                                                       
000207*    03  -COPY WDGX01                                                     
000208     EJECT                                                                
000209 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5110'.                    
000210 01  DLI-IO-WDGX5110.                                                     
000211*    03  -COPY WDGX5110                                                   
000212     EJECT                                                                
000213 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5112'.                    
000214 01  DLI-IO-WDGX5112.                                                     
000215*    03  -COPY WDGX5112                                                   
000216     EJECT                                                                
000217 LINKAGE SECTION.                                                         
000218*01  -COPY W0009   -PRE MSG-                                              
000219*01  -COPY W0008     -PRE WDP7-                                           
000220     05  FILLER           PIC X.                                          
000221                                                                          
000222*01  -COPY W0008  -PRE WDK6-                                              
000223     05  FILLER                  PIC X.                                   
000224                                                                          
000225*01  -COPY W0008  -PRE WDT5-                                              
000226     05  FILLER                  PIC X.                                   
000227                                                                          
000228*01  -COPY W0008  -PRE WDM1-                                              
000229     05  FILLER                  PIC X.                                   
000230*01  -COPY W0008  -PRE BENA-                                              
000231     05  FILLER                  PIC X.                                   
000232     EJECT                                                                
000233*01  -COPY W0008  -PRE WDR2-                                              
000234     05  FILLER                  PIC X.                                   
000235     EJECT                                                                
000236 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB WDT5-PCB             
000237                           WDM1-PCB BENA-PCB WDR2-PCB.                    
000238 MAIN SECTION.                                                            
000239     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB WDT5-PCB             
000240                           WDM1-PCB BENA-PCB WDR2-PCB.                    
000241                                                                          
000242     PERFORM IMS-GET-MSG                                                  
000243     IF SEGMENT-FOUND                                                     
000244       PERFORM A-INIT                                                     
000245       PERFORM B-CHECK-KEYS                                               
000246       IF KEYS-OK                                                         
000247           IF MFS-FIRST                                                   
000248             PERFORM C-FIRST-PAGE                                         
000249           ELSE                                                           
000250             IF MFS-NEXT                                                  
000251               PERFORM D-NEXT-PAGE                                        
000252             ELSE                                                         
000253               PERFORM E-SAME-PAGE                                        
000254             END-IF                                                       
000255           END-IF                                                         
000256         PERFORM F-READ-SHOW-INFO                                         
000257       END-IF                                                             
000258*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
000259*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
000260       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O18601 + 4                      
000261       PERFORM IMS-INSERT-MSG                                             
000262     END-IF                                                               
000263                                                                          
000264     MOVE ZERO TO RETURN-CODE                                             
000265     GOBACK                                                               
000266     .                                                                    
000267     EJECT                                                                
000268 A-INIT SECTION.                                                          
000269                                                                          
000270     IF MSG-DOUBLE-TRANSACTIONS                                           
000271       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I18601                 
000272       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000273       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000274     ELSE                                                                 
000275       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I18601                  
000276       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000277       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000278     END-IF                                                               
000279                                                                          
000280     ACCEPT TODAYS-DATE   FROM DATE                                       
000281                                                                          
000282     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000283     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000284     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000285                                                                          
000286     MOVE LOW-VALUE TO MSG-AREA                                           
000287     MOVE 'W6O186N1' TO MFS-IDMOD                                         
000288     MOVE '6186' TO MOD-IDTRANS                                           
000289     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
000290                                                                          
000291     IF OWN-MID OR HELP-MID                                               
000292       CONTINUE                                                           
000293     ELSE                                                                 
000294       MOVE SPACE TO MFS-KDTRTYP                                          
000295       MOVE '7' TO MFS-IDPFK                                              
000296     END-IF                                                               
000297     .                                                                    
000298     EJECT                                                                
000299 B-CHECK-KEYS SECTION.                                                    
000300                                                                          
000301     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000302     MOVE '001'             TO MSGI-KDCALL                                
000303     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000304     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000305     MOVE '6186'            TO MSGI-IDTRANS                               
000306     IF GOOD-MID                                                          
000307         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
000308         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
000309     END-IF                                                               
000310     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
000311     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
000312                                                                          
000313*    - LANGUAGE TO BE USED BY MEDKONV                                     
000314     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
000315                                                                          
000316     MOVE YES TO KEYS-SW                                                  
000317                                                                          
000318                                                                          
000319*    -- CHECK OF IDARTNR                                                  
000320     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-IN                               
000321                                                                          
000322     IF MID-IDARTNR-IN NOT = ALL '+'                                      
000323       MOVE '7'         TO MFS-IDPFK                                      
000324       MOVE SPACE       TO MFS-KDTRTYP                                    
000325     END-IF                                                               
000326     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
000327     IF MSGI-IDARTNR NUMERIC                                              
000328       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
000329     ELSE                                                                 
000330       MOVE NOO TO KEYS-SW                                                
000331     END-IF                                                               
000332                                                                          
000333                                                                          
000334     IF GOOD-MID OR KEYS-OK                                               
000335       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
000336     ELSE                                                                 
000337       MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-UT                             
000338     END-IF                                                               
000339                                                                          
000340     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
000341                                                                          
000342     IF KEYS-WRONG                                                        
000343       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000344       CALL WMEDKONV USING MED-WMEDAREA                                   
000345       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000346       PERFORM MFS-ERASE-FIELD-IN                                         
000347       PERFORM MFS-ERASE-FIELD-OUT                                        
000348     END-IF                                                               
000349     .                                                                    
000350     EJECT                                                                
000351 C-FIRST-PAGE SECTION.                                                    
000352                                                                          
000353     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
000354     CALL WMEDKONV USING MED-WMEDAREA                                     
000355     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
000356                                                                          
000357     MOVE 0 TO SAVE-UPDAT-ENTER                                           
000358               SAVE-UPDAT-NEXT                                            
000359     PERFORM MFS-ERASE-FIELD-IN                                           
000360     .                                                                    
000361     EJECT                                                                
000362 D-NEXT-PAGE SECTION.                                                     
000363                                                                          
000364     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
000365     IF SAVE-IDTRANS = '6186'                                             
000366       MOVE SAVE-UPDAT-NEXT TO W-DADATTID-9KOMPL-MAX                      
000367     ELSE                                                                 
000368       PERFORM MFS-ERASE-FIELD-IN                                         
000369       MOVE 0 TO SAVE-UPDAT-ENTER                                         
000370                 SAVE-UPDAT-NEXT                                          
000371     END-IF                                                               
000372     .                                                                    
000373     EJECT                                                                
000374 E-SAME-PAGE SECTION.                                                     
000375                                                                          
000376     IF SAVE-IDTRANS = '6186' OR '0551'                                   
000377       MOVE SAVE-UPDAT-ENTER TO W-DADATTID-9KOMPL-MAX                     
000378     ELSE                                                                 
000379       PERFORM MFS-ERASE-FIELD-IN                                         
000380       MOVE 0 TO SAVE-UPDAT-ENTER                                         
000381                 SAVE-UPDAT-NEXT                                          
000382     END-IF                                                               
000383     .                                                                    
000384     EJECT                                                                
000385 F-READ-SHOW-INFO SECTION.                                                
000386                                                                          
000387     PERFORM FA-READ-BASICDATA                                            
000388                                                                          
000389     IF SEGMENT-MISSING                                                   
000390        MOVE PARTNO-MISSING TO MED-IDMFSFEL                               
000391        CALL WMEDKONV USING MED-WMEDAREA                                  
000392        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
000393        PERFORM MFS-ERASE-FIELD-OUT                                       
000394     ELSE                                                                 
000395       PERFORM IMS-GU-BENA11                                              
000396       IF SEGMENT-FOUND                                                   
000397         MOVE TEXT-BEART TO MOD-BEART                                     
000398       ELSE                                                               
000399         MOVE SPACE             TO MOD-BEART                              
000400       END-IF                                                             
000401                                                                          
000402****** FILL VALUES OF PREFERENTIAL STATUS TRADE GROUP                     
000403                                                                          
000404       MOVE ' ' TO  MOD-FLPCOO-TGA                                        
000405                    MOD-FLPCOO-TGB                                        
000406                    MOD-FLPCOO-TGC                                        
000407                    MOD-FLPCOO-TGD                                        
000408                    MOD-FLPCOO-TGE                                        
000409                    MOD-FLPCOO-TGF                                        
000410                    MOD-FLPCOO-TGG                                        
000411                    MOD-FLPCOO-TGH                                        
000412                    MOD-FLPCOO-TGI                                        
000413                    MOD-FLPCOO-TGJ                                        
000414                    MOD-FLPCOO-TGK                                        
000415                    MOD-FLPCOO-TGL                                        
000416                    MOD-FLPCOO-TGM                                        
000417                    MOD-FLPCOO-TGN                                        
000418                    MOD-FLPCOO-TGO                                        
000419                    MOD-FLPCOO-TGP                                        
000420                    MOD-FLPCOO-TGQ                                        
000421                    MOD-FLPCOO-TGR                                        
000422                    MOD-FLPCOO-TGS                                        
000423                    MOD-FLPCOO-TGT                                        
000424                    MOD-FLPCOO-TGU                                        
000425                    MOD-FLPCOO-TGV                                        
000426                    MOD-FLPCOO-TGW                                        
000427                    MOD-FLPCOO-TGX                                        
000428                    MOD-FLPCOO-TGY                                        
000429                    MOD-FLPCOO-TGZ                                        
000430                    MOD-TG1                                               
000431                    MOD-TG2                                               
000432                    MOD-TG3                                               
000433                    MOD-TG4                                               
000434                    MOD-TG5                                               
000435                    MOD-TG6                                               
000436                    MOD-TG7                                               
000437                    MOD-TG8                                               
000438                    MOD-TG9                                               
000439                    MOD-TG10                                              
000440                    MOD-TG11                                              
000441                    MOD-TG12                                              
000442                    MOD-TG13                                              
000443                    MOD-TG14                                              
000444                    MOD-TG15                                              
000445                    MOD-TG16                                              
000446                    MOD-TG17                                              
000447                    MOD-TG18                                              
000448                    MOD-TRDF1                                             
000449                    MOD-TRDG1                                             
000450                    MOD-TRDF2                                             
000451                    MOD-TRDG2                                             
000452                    MOD-TRDF3                                             
000453                    MOD-TRDG3                                             
000454                    MOD-TRDF4                                             
000455                    MOD-TRDG4                                             
000456                    MOD-TRDF5                                             
000457                    MOD-TRDG5                                             
000458                    MOD-TRDF6                                             
000459                    MOD-TRDG6                                             
000460                    MOD-TRDF7                                             
000461                    MOD-TRDG7                                             
000462                    MOD-TRDF8                                             
000463                    MOD-TRDG8                                             
000464                    MOD-TRDF9                                             
000465                    MOD-TRDG9                                             
000466                    MOD-TRDF10                                            
000467                    MOD-TRDG10                                            
000468                    MOD-TRDF11                                            
000469                    MOD-TRDG11                                            
000470                    MOD-TRDF12                                            
000471                    MOD-TRDG12                                            
000472                    MOD-TRDF13                                            
000473                    MOD-TRDG13                                            
000474                    MOD-TRDF14                                            
000475                    MOD-TRDG14                                            
000476                    MOD-TRDF15                                            
000477                    MOD-TRDG15                                            
000478                    MOD-TRDF16                                            
000479                    MOD-TRDG16                                            
000480                    MOD-TRDF17                                            
000481                    MOD-TRDG17                                            
000482                    MOD-TRDF18                                            
000483                    MOD-TRDG18                                            
000484                                                                          
000485                    MOD-FLPCOO-TGA                                        
000486                    MOD-FLPCOO-TGB                                        
000487                    MOD-FLPCOO-TGC                                        
000488                    MOD-FLPCOO-TGD                                        
000489                    MOD-FLPCOO-TGE                                        
000490                    MOD-FLPCOO-TGF                                        
000491                    MOD-FLPCOO-TGG                                        
000492                    MOD-FLPCOO-TGH                                        
000493                    MOD-FLPCOO-TGI                                        
000494                    MOD-FLPCOO-TGJ                                        
000495                    MOD-FLPCOO-TGK                                        
000496                    MOD-FLPCOO-TGL                                        
000497                    MOD-FLPCOO-TGM                                        
000498                    MOD-FLPCOO-TGN                                        
000499                    MOD-FLPCOO-TGO                                        
000500                    MOD-FLPCOO-TGP                                        
000501                    MOD-FLPCOO-TGQ                                        
000502                    MOD-FLPCOO-TGR                                        
000503                    MOD-FLPCOO-TGS                                        
000504                    MOD-FLPCOO-TGT                                        
000505                    MOD-FLPCOO-TGU                                        
000506                    MOD-FLPCOO-TGV                                        
000507                    MOD-FLPCOO-TGW                                        
000508                    MOD-FLPCOO-TGX                                        
000509                    MOD-FLPCOO-TGY                                        
000510                    MOD-FLPCOO-TGZ                                        
000511                                                                          
000512       MOVE '       ' TO MOD-IDUSER                                       
000513       MOVE 'N'  TO MOD-KDPCOO                                            
000514       PERFORM FCA-BEAVTAL-TABLE                                          
000515*****! PERFORM IMS----WDM1!                                               
000516       MOVE ART-IDLEVNR      TO W-IDLEVNR-WDM1                            
000517                                MOD-IDLEVNR                               
000518       MOVE CLAG-KDARTURS    TO MOD-KDARTURS-PCOO                         
000519       MOVE W-IDARTNR        TO W-IDARTNR-WDM1                            
000520       PERFORM IMS-GET-WDM101                                             
000521       IF SEGMENT-FOUND                                                   
000522         MOVE ARTU-KVLS-PCOO TO MOD-KVLS                                  
000524         MOVE LOW-VALUE             TO W-BEAVTAL-MIN                      
000525         MOVE HIGH-VALUE            TO W-BEAVTAL-MAX                      
000526         PERFORM IMS-GU-WDM111                                            
000527         PERFORM UNTIL SEGMENT-MISSING                                    
000528           IF SEGMENT-FOUND                                               
000529                                                                          
000530              PERFORM IMS-GNP-WDM121                                      
000531              PERFORM UNTIL SEGMENT-MISSING                               
000532                IF  TODAYS-DATE  > STAV-TIGILTIG-FOM                      
000533                AND TODAYS-DATE  < STAV-TIGILTIG-TOM                      
000534                  IF STAV-IDSTAMIC = 1  AND                               
000535                  STAV-KDARTURS-PCOO =  CLAG-KDARTURS                     
000536                    IF NOT-FIRST-PCOO                                     
000539                       MOVE NOO TO PCOO-FIRST-SW                          
000541                       MOVE 'Y'  TO MOD-KDPCOO                            
000542                       MOVE STAV-TIGILTIG-FOM TO MOD-TIGILTIG-FOM         
000543                       MOVE STAV-TIGILTIG-TOM TO MOD-TIGILTIG-TOM         
000544                    END-IF                                                
000545                    PERFORM FC-VALIDATE-BEAVTAL                           
000546                  END-IF                                                  
000547                END-IF                                                    
000548                PERFORM IMS-GNP-WDM121                                    
000549              END-PERFORM                                                 
000550           END-IF                                                         
000551         PERFORM IMS-GN-WDM111                                            
000552         END-PERFORM                                                      
000553       END-IF                                                             
000554                                                                          
000555                                                                          
000556       PERFORM IMS-GU-WDT501                                              
000557       IF SEGMENT-FOUND                                                   
000558         MOVE +1 TO INDX                                                  
000559                    INDX-DET                                              
000560         IF MFS-FIRST                                                     
000561** READ FOR ONLY MFS-FIRST!!                                              
000562           MOVE 0 TO SAVE-UPDAT-ENTER                                     
000563                     SAVE-UPDAT-NEXT                                      
000564                                                                          
000565           PERFORM FB-READ-LINEDATA                                       
000566           IF SEGMENT-FOUND                                               
000567             PERFORM FB-READ-LINEDATA                                     
000568             PERFORM UNTIL  INDX-DET   > MAX-INDX                         
000569               IF SEGMENT-FOUND                                           
000570                 MOVE GLO-KDARTURS TO                                     
000571                          MOD-KDARTURS-PCOO-DET (INDX-DET)                
000572                 MOVE GLO-IDLEVNR TO                                      
000573                          MOD-IDLEVNR-DET (INDX-DET)                      
000574                 MOVE GLO-IDUSER   TO                                     
000575                          MOD-IDUSER-DET (INDX-DET)                       
000576                 COMPUTE W-DADATTID-9KOMPL =                              
000577                 99999999999999  - GLO-DADATTID-9KOMPL                    
000578                 MOVE W-DADATTID-9KOMPL(3:6)   TO                         
000579                      MOD-TIREGDAT-DET (INDX-DET)                         
000580                 MOVE GLO-DADATTID-9KOMPL TO W-UPDAT-MIN                  
000581                 IF INDX-DET = 1                                          
000582                  MOVE W-UPDAT-MIN   TO SAVE-UPDAT-ENTER                  
000583                 END-IF                                                   
000584                 PERFORM FB-READ-LINEDATA                                 
000585                 ADD 1 TO INDX-DET                                        
000586               ELSE                                                       
000587                 ADD 1 TO INDX-DET                                        
000588               END-IF                                                     
000589             END-PERFORM                                                  
000590           END-IF                                                         
000591         ELSE                                                             
000592** READ OTHER THAN MFS-FIRST!!                                            
000593           PERFORM FD-READ-DETINFO                                        
000594           IF SEGMENT-FOUND                                               
000595            MOVE W-DADATTID-9KOMPL TO W-DADATTID-9KOMPL-COM               
000596           END-IF                                                         
000597                                                                          
000598           IF MFS-NEXT                                                    
000599             IF SAVE-IDTRANS = '6186'                                     
000600               MOVE SAVE-UPDAT-NEXT TO W-DADATTID-9KOMPL-MAX              
000601             END-IF                                                       
000602           ELSE                                                           
000603             IF SAVE-IDTRANS = '6186' OR '0551'                           
000604               MOVE SAVE-UPDAT-ENTER TO W-DADATTID-9KOMPL-MAX             
000605             END-IF                                                       
000606           END-IF                                                         
000607                                                                          
000608           PERFORM FB-READ-LINEDATA                                       
000609           IF SEGMENT-FOUND                                               
000610                                                                          
000611             COMPUTE W-DADATTID-9KOMPL =                                  
000612             99999999999999  - GLO-DADATTID-9KOMPL                        
000613                                                                          
000614             IF W-DADATTID-9KOMPL = W-DADATTID-9KOMPL-COM                 
000615               PERFORM FB-READ-LINEDATA                                   
000616             END-IF                                                       
000617                                                                          
000618             PERFORM UNTIL  INDX-DET   > MAX-INDX                         
000619               IF SEGMENT-FOUND                                           
000620                 MOVE GLO-KDARTURS TO                                     
000621                          MOD-KDARTURS-PCOO-DET (INDX-DET)                
000622                 MOVE GLO-IDLEVNR TO                                      
000623                          MOD-IDLEVNR-DET (INDX-DET)                      
000624                 MOVE GLO-IDUSER   TO                                     
000625                          MOD-IDUSER-DET (INDX-DET)                       
000626                 COMPUTE W-DADATTID-9KOMPL =                              
000627                 99999999999999  - GLO-DADATTID-9KOMPL                    
000628                 MOVE W-DADATTID-9KOMPL(3:6)   TO                         
000629                      MOD-TIREGDAT-DET (INDX-DET)                         
000630                 MOVE GLO-DADATTID-9KOMPL TO W-UPDAT-MIN                  
000631                 IF INDX-DET = 1                                          
000632                  MOVE W-UPDAT-MIN   TO SAVE-UPDAT-ENTER                  
000633                 END-IF                                                   
000634                 PERFORM FB-READ-LINEDATA                                 
000635                 ADD 1 TO INDX-DET                                        
000636               ELSE                                                       
000637                 ADD 1 TO INDX-DET                                        
000638               END-IF                                                     
000639             END-PERFORM                                                  
000640           END-IF                                                         
000641         END-IF                                                           
000642                                                                          
000643                                                                          
000644         IF SEGMENT-FOUND                                                 
000645           MOVE GLO-DADATTID-9KOMPL TO W-UPDAT-MIN                        
000646           MOVE W-UPDAT-MIN TO SAVE-UPDAT-NEXT                            
000647           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
000648           CALL WMEDKONV USING MED-WMEDAREA                               
000649           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
000650         ELSE                                                             
000651           MOVE SAVE-UPDAT-ENTER TO SAVE-UPDAT-NEXT                       
000652           IF MFS-NEXT                                                    
000653             MOVE INF-NO-MORE-INFO     TO MED-IDMFSINF                    
000654             CALL WMEDKONV USING MED-WMEDAREA                             
000655             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
000656           END-IF                                                         
000657         END-IF                                                           
000658       END-IF                                                             
000659                                                                          
000660       PERFORM FD-READ-DETINFO                                            
000661                                                                          
000662       MOVE '002'      TO MSGI-KDCALL                                     
000663       MOVE '6186'     TO MSGI-IDTRANS                                    
000664                          SAVE-IDTRANS                                    
000665       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
000666       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
000667     END-IF                                                               
000668     .                                                                    
000669     EJECT                                                                
000670 FA-READ-BASICDATA SECTION.                                               
000671     PERFORM IMS-GU-WDK6-WDK601                                           
000672     IF SEGMENT-FOUND                                                     
000673       PERFORM IMS-GU-WDK6-WDK611                                         
000674     END-IF                                                               
000675                                                                          
000676     .                                                                    
000677     EJECT                                                                
000678 FB-READ-LINEDATA SECTION.                                                
000679                                                                          
000680     PERFORM IMS-GNP-WDT511                                               
000681                                                                          
000682                                                                          
000683     .                                                                    
000684     EJECT                                                                
000685 FCA-BEAVTAL-TABLE SECTION.                                               
000686                                                                          
000687     PERFORM IMS-GN-WDGX5110                                              
000688     PERFORM                                                              
000689       UNTIL SEGMENT-MISSING                                              
000690       MOVE 5110-BEAVTAL         TO WS-BEAVTAL                            
000691       ADD 1 TO WS-COUNT-BEVTAL                                           
000692       EVALUATE WS-COUNT-BEVTAL                                           
000693         WHEN 1                                                           
000694              MOVE 'A=' TO MOD-TRDF1                                      
000695              MOVE 'A'  TO MOD-TG1                                        
000696              MOVE 'N'  TO MOD-FLPCOO-TGA                                 
000697              MOVE WS-BEAVTAL TO MOD-TRDG1                                
000698         WHEN 2                                                           
000699              MOVE 'B=' TO MOD-TRDF2                                      
000700              MOVE 'B'  TO MOD-TG2                                        
000701              MOVE 'N'  TO MOD-FLPCOO-TGB                                 
000702              MOVE WS-BEAVTAL TO MOD-TRDG2                                
000703         WHEN 3                                                           
000704              MOVE 'C=' TO MOD-TRDF3                                      
000705              MOVE 'C'  TO MOD-TG3                                        
000706              MOVE 'N'  TO MOD-FLPCOO-TGC                                 
000707              MOVE WS-BEAVTAL TO MOD-TRDG3                                
000708         WHEN 4                                                           
000709              MOVE 'D=' TO MOD-TRDF4                                      
000710              MOVE 'D'  TO MOD-TG4                                        
000711              MOVE 'N'  TO MOD-FLPCOO-TGD                                 
000712              MOVE WS-BEAVTAL TO MOD-TRDG4                                
000713         WHEN 5                                                           
000714              MOVE 'E=' TO MOD-TRDF5                                      
000715              MOVE 'E'  TO MOD-TG5                                        
000716              MOVE 'N'  TO MOD-FLPCOO-TGE                                 
000717              MOVE WS-BEAVTAL TO MOD-TRDG5                                
000718         WHEN 6                                                           
000719              MOVE 'F=' TO MOD-TRDF6                                      
000720              MOVE 'F'  TO MOD-TG6                                        
000721              MOVE 'N'  TO MOD-FLPCOO-TGF                                 
000722              MOVE WS-BEAVTAL TO MOD-TRDG6                                
000723         WHEN 7                                                           
000724              MOVE 'G=' TO MOD-TRDF7                                      
000725              MOVE 'G'  TO MOD-TG7                                        
000726              MOVE 'N'  TO MOD-FLPCOO-TGG                                 
000727              MOVE WS-BEAVTAL TO MOD-TRDG7                                
000728         WHEN 8                                                           
000729              MOVE 'H=' TO MOD-TRDF8                                      
000730              MOVE 'H'  TO MOD-TG8                                        
000731              MOVE 'N'  TO MOD-FLPCOO-TGH                                 
000732              MOVE WS-BEAVTAL TO MOD-TRDG8                                
000733         WHEN 9                                                           
000734              MOVE 'I=' TO MOD-TRDF9                                      
000735              MOVE 'I'  TO MOD-TG9                                        
000736              MOVE 'N'  TO MOD-FLPCOO-TGI                                 
000737              MOVE WS-BEAVTAL TO MOD-TRDG9                                
000738         WHEN 10                                                          
000739              MOVE 'J=' TO MOD-TRDF10                                     
000740              MOVE 'J'  TO MOD-TG10                                       
000741              MOVE 'N'  TO MOD-FLPCOO-TGJ                                 
000742              MOVE WS-BEAVTAL TO MOD-TRDG10                               
000743         WHEN 11                                                          
000744              MOVE 'K=' TO MOD-TRDF11                                     
000745              MOVE 'K'  TO MOD-TG11                                       
000746              MOVE 'N'  TO MOD-FLPCOO-TGK                                 
000747              MOVE WS-BEAVTAL TO MOD-TRDG11                               
000748         WHEN 12                                                          
000749              MOVE 'L=' TO MOD-TRDF12                                     
000750              MOVE 'L'  TO MOD-TG12                                       
000751              MOVE 'N'  TO MOD-FLPCOO-TGL                                 
000752              MOVE WS-BEAVTAL TO MOD-TRDG12                               
000753         WHEN 13                                                          
000754              MOVE 'M=' TO MOD-TRDF13                                     
000755              MOVE 'M'  TO MOD-TG13                                       
000756              MOVE 'N'  TO MOD-FLPCOO-TGM                                 
000757              MOVE WS-BEAVTAL TO MOD-TRDG13                               
000758         WHEN 14                                                          
000759              MOVE 'N=' TO MOD-TRDF14                                     
000760              MOVE 'N'  TO MOD-TG14                                       
000761              MOVE 'N'  TO MOD-FLPCOO-TGN                                 
000762              MOVE WS-BEAVTAL TO MOD-TRDG14                               
000763         WHEN 15                                                          
000764              MOVE 'O=' TO MOD-TRDF15                                     
000765              MOVE 'O'  TO MOD-TG15                                       
000766              MOVE 'N'  TO MOD-FLPCOO-TGO                                 
000767              MOVE WS-BEAVTAL TO MOD-TRDG15                               
000768         WHEN 16                                                          
000769              MOVE 'P=' TO MOD-TRDF16                                     
000770              MOVE 'P'  TO MOD-TG16                                       
000771              MOVE 'N'  TO MOD-FLPCOO-TGP                                 
000772              MOVE WS-BEAVTAL TO MOD-TRDG16                               
000773         WHEN 17                                                          
000774              MOVE 'Q=' TO MOD-TRDF17                                     
000775              MOVE 'Q'  TO MOD-TG17                                       
000776              MOVE 'N'  TO MOD-FLPCOO-TGQ                                 
000777              MOVE WS-BEAVTAL TO MOD-TRDG17                               
000778         WHEN 18                                                          
000779              MOVE 'R=' TO MOD-TRDF18                                     
000780              MOVE 'R'  TO MOD-TG18                                       
000781              MOVE 'N'  TO MOD-FLPCOO-TGR                                 
000782              MOVE WS-BEAVTAL TO MOD-TRDG18                               
000783       END-EVALUATE                                                       
000784                                                                          
000785       PERFORM IMS-GN-WDGX5110                                            
000786     END-PERFORM                                                          
000787     .                                                                    
000788     EJECT                                                                
000789 FC-VALIDATE-BEAVTAL SECTION.                                             
000790                                                                          
000791     IF AVT-BEAVTAL = MOD-TRDG1                                           
000792       MOVE 'Y'  TO MOD-FLPCOO-TGA                                        
000793     END-IF                                                               
000794                                                                          
000795     IF AVT-BEAVTAL = MOD-TRDG2                                           
000796       MOVE 'Y'  TO MOD-FLPCOO-TGB                                        
000797     END-IF                                                               
000798                                                                          
000799     IF AVT-BEAVTAL = MOD-TRDG3                                           
000800       MOVE 'Y'  TO MOD-FLPCOO-TGC                                        
000801     END-IF                                                               
000802                                                                          
000803     IF AVT-BEAVTAL = MOD-TRDG4                                           
000804       MOVE 'Y'  TO MOD-FLPCOO-TGD                                        
000805     END-IF                                                               
000806                                                                          
000807     IF AVT-BEAVTAL = MOD-TRDG5                                           
000808       MOVE 'Y'  TO MOD-FLPCOO-TGE                                        
000809     END-IF                                                               
000810                                                                          
000811     IF AVT-BEAVTAL = MOD-TRDG6                                           
000812       MOVE 'Y'  TO MOD-FLPCOO-TGF                                        
000813     END-IF                                                               
000814                                                                          
000815     IF AVT-BEAVTAL = MOD-TRDG7                                           
000816       MOVE 'Y'  TO MOD-FLPCOO-TGG                                        
000817     END-IF                                                               
000818                                                                          
000819     IF AVT-BEAVTAL = MOD-TRDG8                                           
000820       MOVE 'Y'  TO MOD-FLPCOO-TGH                                        
000821     END-IF                                                               
000822                                                                          
000823     IF AVT-BEAVTAL = MOD-TRDG9                                           
000824       MOVE 'Y'  TO MOD-FLPCOO-TGI                                        
000825     END-IF                                                               
000826                                                                          
000827     IF AVT-BEAVTAL = MOD-TRDG10                                          
000828       MOVE 'Y'  TO MOD-FLPCOO-TGJ                                        
000829     END-IF                                                               
000830                                                                          
000831     IF AVT-BEAVTAL = MOD-TRDG11                                          
000832       MOVE 'Y'  TO MOD-FLPCOO-TGK                                        
000833     END-IF                                                               
000834                                                                          
000835     IF AVT-BEAVTAL = MOD-TRDG12                                          
000836       MOVE 'Y'  TO MOD-FLPCOO-TGL                                        
000837     END-IF                                                               
000838                                                                          
000839     IF AVT-BEAVTAL = MOD-TRDG13                                          
000840       MOVE 'Y'  TO MOD-FLPCOO-TGM                                        
000841     END-IF                                                               
000842                                                                          
000843     IF AVT-BEAVTAL = MOD-TRDG14                                          
000844       MOVE 'Y'  TO MOD-FLPCOO-TGN                                        
000845     END-IF                                                               
000846                                                                          
000847     IF AVT-BEAVTAL = MOD-TRDG15                                          
000848       MOVE 'Y'  TO MOD-FLPCOO-TGO                                        
000849     END-IF                                                               
000850                                                                          
000851     IF AVT-BEAVTAL = MOD-TRDG16                                          
000852       MOVE 'Y'  TO MOD-FLPCOO-TGP                                        
000853     END-IF                                                               
000854                                                                          
000855     IF AVT-BEAVTAL = MOD-TRDG17                                          
000856       MOVE 'Y'  TO MOD-FLPCOO-TGQ                                        
000857     END-IF                                                               
000858                                                                          
000859     IF AVT-BEAVTAL = MOD-TRDG18                                          
000860       MOVE 'Y'  TO MOD-FLPCOO-TGR                                        
000861     END-IF                                                               
000862                                                                          
000863     .                                                                    
000864     EJECT                                                                
000865                                                                          
000866 FD-READ-DETINFO SECTION.                                                 
000867                                                                          
000868     MOVE 99999999999999  TO W-DADATTID-9KOMPL-MIN                        
000869     MOVE 00000000000000  TO W-DADATTID-9KOMPL-MAX                        
000870     PERFORM IMS-GU-WDT501                                                
000871     IF SEGMENT-FOUND                                                     
000872       PERFORM FB-READ-LINEDATA                                           
000873       IF SEGMENT-FOUND                                                   
000874        MOVE GLO-IDUSER   TO MOD-IDUSER                                   
000875        COMPUTE W-DADATTID-9KOMPL =                                       
000876        99999999999999  - GLO-DADATTID-9KOMPL                             
000877        MOVE W-DADATTID-9KOMPL(3:6)   TO                                  
000878             MOD-TIREGDAT                                                 
000879                                                                          
000880       END-IF                                                             
000881     END-IF                                                               
000882     .                                                                    
000883     EJECT                                                                
000884                                                                          
000885 MFS-ERASE-FIELD-OUT SECTION.                                             
000886                                                                          
000887*    --- INCL. SCROLL KEYS                                                
000888*    MOVE MFS-ERASE-FIELD TO MOD-W6O18601                                 
000889     MOVE 0   TO  SAVE-UPDAT-ENTER                                        
000890                  SAVE-UPDAT-NEXT                                         
000891                                                                          
000892     MOVE MFS-ERASE-FIELD                                                 
000893              TO   MOD-BEART                                              
000894                   MOD-KDARTURS-PCOO                                      
000895                   MOD-KDPCOO                                             
000896                   MOD-KVLS                                               
000897                   MOD-IDLEVNR                                            
000898                   MOD-IDUSER                                             
000899                   MOD-TIREGDAT                                           
000900                   MOD-TIGILTIG-FOM                                       
000901                   MOD-TIGILTIG-TOM                                       
000902                   MOD-FLPCOO-TGA                                         
000903                   MOD-FLPCOO-TGB                                         
000904                   MOD-FLPCOO-TGC                                         
000905                   MOD-FLPCOO-TGD                                         
000906                   MOD-FLPCOO-TGE                                         
000907                   MOD-FLPCOO-TGF                                         
000908                   MOD-FLPCOO-TGG                                         
000909                   MOD-FLPCOO-TGH                                         
000910                   MOD-FLPCOO-TGI                                         
000911                   MOD-FLPCOO-TGJ                                         
000912                   MOD-FLPCOO-TGK                                         
000913                   MOD-FLPCOO-TGL                                         
000914                   MOD-FLPCOO-TGM                                         
000915                   MOD-FLPCOO-TGN                                         
000916                   MOD-FLPCOO-TGO                                         
000917                   MOD-FLPCOO-TGP                                         
000918                   MOD-FLPCOO-TGQ                                         
000919                   MOD-FLPCOO-TGR                                         
000920                   MOD-FLPCOO-TGS                                         
000921                   MOD-FLPCOO-TGT                                         
000922                   MOD-FLPCOO-TGU                                         
000923                   MOD-FLPCOO-TGV                                         
000924                   MOD-FLPCOO-TGW                                         
000925                   MOD-FLPCOO-TGX                                         
000926                   MOD-FLPCOO-TGY                                         
000927                   MOD-FLPCOO-TGZ                                         
000928     MOVE +1 TO INDX                                                      
000929     PERFORM UNTIL INDX > MAX-INDX                                        
000930       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
000931       ADD +1 TO INDX                                                     
000932     END-PERFORM                                                          
000933     .                                                                    
000934     SKIP3                                                                
000935 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
000936                                                                          
000937*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
000938     MOVE MFS-ERASE-FIELD                                                 
000939                      TO MOD-KDARTURS-PCOO-DET (INDX)                     
000940                         MOD-IDUSER-DET (INDX)                            
000941                         MOD-TIREGDAT-DET (INDX)                          
000942     .                                                                    
000943     SKIP3                                                                
000944 MFS-ERASE-FIELD-IN SECTION.                                              
000945                                                                          
000946     MOVE MFS-ERASE-FIELD TO MID-IDARTNR-IN                               
000947     .                                                                    
000948     EJECT                                                                
000949* --- IMS SECTIONS ---                                                    
000950     SKIP3                                                                
000951 IMS-GET-MSG SECTION.                                                     
000952                                                                          
000953     MOVE '  QC' TO GOOD-STATUSCODES                                      
000954     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000955     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000956     PERFORM IMS-STATUSCHECK                                              
000957     .                                                                    
000958     SKIP3                                                                
000959 IMS-INSERT-MSG SECTION.                                                  
000960                                                                          
000961     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000962     MOVE SPACE TO GOOD-STATUSCODES                                       
000963     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000964     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000965     PERFORM IMS-STATUSCHECK                                              
000966     .                                                                    
000967     EJECT                                                                
000968 IMS-GU-WDK6-WDK601 SECTION.                                              
000969*WDK6                                                                     
000970     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
000971            DELIMITED BY SIZE INTO SSA1                                   
000972     MOVE '  GE' TO GOOD-STATUSCODES                                      
000973     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
000974     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
000975     PERFORM IMS-STATUSCHECK                                              
000976     .                                                                    
000977     EJECT                                                                
000978 IMS-GU-WDK6-WDK611 SECTION.                                              
000979                                                                          
000980     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
000981          DELIMITED BY SIZE INTO SSA1                                     
000982     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
000983          DELIMITED BY SIZE INTO SSA2                                     
000984     MOVE '  GE' TO GOOD-STATUSCODES                                      
000985     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
000986     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
000987     PERFORM IMS-STATUSCHECK                                              
000988     .                                                                    
000989     SKIP3                                                                
000990 IMS-GU-WDT501 SECTION.                                                   
000991     STRING 'WDT501  (IDARTNR = ' W-IDARTNR-X ')'                         
000992          DELIMITED BY SIZE INTO SSA1                                     
000993     MOVE '  GE' TO GOOD-STATUSCODES                                      
000994     CALL CBLTDLI USING GU  WDT5-PCB DLI-IO-WDT501 SSA1                   
001000     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
001001     PERFORM IMS-STATUSCHECK                                              
001002     .                                                                    
001003     EJECT                                                                
001004 IMS-GNP-WDT511 SECTION.                                                  
001005                                                                          
001006     STRING 'WDT511  (DADATTI9<=' W-WDT511-KEY-MIN-X                      
001007                    '&DADATTI9>=' W-WDT511-KEY-MAX-X ')'                  
001008            DELIMITED BY SIZE INTO SSA1                                   
001009     MOVE '  GE' TO GOOD-STATUSCODES                                      
001010     CALL CBLTDLI USING GNP WDT5-PCB DLI-IO-WDT511 SSA1                   
001011     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
001012     PERFORM IMS-STATUSCHECK                                              
001013     .                                                                    
001014     EJECT                                                                
001015 IMS-GU-BENA11 SECTION.                                                   
001016     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
001017            DELIMITED BY SIZE INTO SSA1                                   
001018     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
001019            DELIMITED BY SIZE INTO SSA2                                   
001020     MOVE '  GE' TO GOOD-STATUSCODES                                      
001021     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
001022     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
001023     PERFORM IMS-STATUSCHECK                                              
001024     .                                                                    
001025     EJECT                                                                
001026 IMS-GET-WDM101 SECTION.                                                  
001027                                                                          
001028     STRING 'WDM101  (WDM101KY =' W-WDM101KY-X ')'                        
001029          DELIMITED BY SIZE INTO SSA1                                     
001030     MOVE '  GE' TO GOOD-STATUSCODES                                      
001031     CALL CBLTDLI USING GU WDM1-PCB DLI-IO-WDM101 SSA1                    
001032     MOVE WDM1-STATUS-CODE TO STATUS-WS                                   
001033     PERFORM IMS-STATUSCHECK                                              
001034     .                                                                    
001035     EJECT                                                                
001036 IMS-GU-WDM111 SECTION.                                                   
001037     STRING 'WDM101  (WDM101KY =' W-WDM101KY-X ')'                        
001038          DELIMITED BY SIZE INTO SSA1                                     
001039     STRING 'WDM111  (BEAVTAL >=' W-BEAVTAL-MIN-X                         
001040                    '&BEAVTAL <=' W-BEAVTAL-MAX-X ')'                     
001050          DELIMITED BY SIZE INTO SSA2                                     
001060     MOVE '  GE'                 TO GOOD-STATUSCODES                      
001070     CALL CBLTDLI USING GU WDM1-PCB DLI-IO-WDM111 SSA1 SSA2               
001080     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
001090     PERFORM IMS-STATUSCHECK                                              
001100     .                                                                    
001200     SKIP3                                                                
001300                                                                          
001400 IMS-GN-WDM111 SECTION.                                                   
001500     STRING 'WDM101  (WDM101KY =' W-WDM101KY-X ')'                        
001600          DELIMITED BY SIZE INTO SSA1                                     
001700     STRING 'WDM111  (BEAVTAL >=' W-BEAVTAL-MIN-X                         
001800                    '&BEAVTAL <=' W-BEAVTAL-MAX-X ')'                     
001900          DELIMITED BY SIZE INTO SSA2                                     
002000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
002100     CALL CBLTDLI USING GN WDM1-PCB DLI-IO-WDM111 SSA1 SSA2               
002200     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
002300     PERFORM IMS-STATUSCHECK                                              
002400     .                                                                    
002500     SKIP3                                                                
002501                                                                          
002502 IMS-GNP-WDM111 SECTION.                                                  
002503     MOVE   'WDM111 ' TO SSA1                                             
002504     MOVE '  GE'                 TO GOOD-STATUSCODES                      
002505     CALL CBLTDLI USING GNP WDM1-PCB DLI-IO-WDM111 SSA1                   
002506     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
002507     PERFORM IMS-STATUSCHECK                                              
002508     .                                                                    
002509     SKIP3                                                                
002510                                                                          
002511 IMS-GNP-WDM121 SECTION.                                                  
002512     MOVE   'WDM121 ' TO SSA1                                             
002513     MOVE '  GE'                 TO GOOD-STATUSCODES                      
002514     CALL CBLTDLI USING GNP WDM1-PCB DLI-IO-WDM121 SSA1                   
002515     MOVE WDM1-STATUS-CODE       TO STATUS-WS                             
002516     PERFORM IMS-STATUSCHECK                                              
002517     .                                                                    
002518     SKIP3                                                                
002519                                                                          
002520 IMS-GET-WDM111 SECTION.                                                  
002521                                                                          
002522     STRING 'WDM101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
002523          DELIMITED BY SIZE INTO SSA1                                     
002524     MOVE '  GE' TO GOOD-STATUSCODES                                      
002525     CALL CBLTDLI USING GU WDM1-PCB DLI-IO-WDM101 SSA1                    
002526     MOVE WDM1-STATUS-CODE TO STATUS-WS                                   
002527     PERFORM IMS-STATUSCHECK                                              
002528     .                                                                    
002529     EJECT                                                                
002530 IMS-GET-WDM121 SECTION.                                                  
002531                                                                          
002532     STRING 'WDM111  (BEAVTAL  =' W-BEAVTAL-X ')'                         
002533          DELIMITED BY SIZE INTO SSA1                                     
002534     MOVE '  GE' TO GOOD-STATUSCODES                                      
002535     CALL CBLTDLI USING GNP WDM1-PCB DLI-IO-WDM111 SSA1                   
002536     MOVE WDM1-STATUS-CODE TO STATUS-WS                                   
002537     PERFORM IMS-STATUSCHECK                                              
002538     .                                                                    
002539     EJECT                                                                
002540 IMS-GN-WDGX5110 SECTION.                                                 
002541                                                                          
002542     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY01-X ')'                       
002543          DELIMITED BY SIZE INTO SSA1                                     
002544     MOVE 'WDGX5110 '            TO SSA2                                  
002545     MOVE '  GE'                 TO GOOD-STATUSCODES                      
002546     CALL CBLTDLI USING GN WDR2-PCB DLI-IO-WDGX5110 SSA1 SSA2             
002547     MOVE WDR2-STATUS-CODE       TO STATUS-WS                             
002548     PERFORM IMS-STATUSCHECK                                              
002549     .                                                                    
002550     EJECT                                                                
002551 IMS-STATUSCHECK SECTION.                                                 
002552                                                                          
002553     SET STATUS-IX TO 1                                                   
002554     SEARCH GOOD-STATUS                                                   
002555       AT END                                                             
002556         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
002557         DELIMITED BY SIZE INTO ERROR-TEXT                                
002558         CALL FELLOG                                                      
002559       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
002560         CONTINUE                                                         
002561     END-SEARCH                                                           
002562     .                                                                    
