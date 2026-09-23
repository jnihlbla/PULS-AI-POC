000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W5616700.                                                
000003 AUTHOR.         ARCHANA BHAT.                                            
000004 DATE-WRITTEN.   20171102.                                                
000005                                                                          
000006*    FUNKTION:                                                            
000007*        LÄSER INFIL, MATCHAR DEN EMOT REGELVERKET OCH SKICKAR            
000008*        EN POST PER KLIENT.                                              
000009*        SKICKAR OCKSÅ EN FIL TILL LAGERVÄRDERINGSLISTOR                  
000010*        SKAPAR OCKSÅ EN FIL FÖR DIREKTLEVERANSLISTA                      
000011*                                                                         
000012*        PROGRAMMET LÄSER      WDH5                                       
000013*                                                                         
000014*    ABENDKODER:                                                          
000015*        U0016 -  . . . .                                                 
000016*        U1000 -  . . . .                                                 
000017     SKIP3                                                                
000018 ENVIRONMENT DIVISION.                                                    
000019     SKIP2                                                                
000020 INPUT-OUTPUT SECTION.                                                    
000021                                                                          
000022 FILE-CONTROL.                                                            
000023     SKIP2                                                                
000024*          --- INFIL MED RÄTTA POSTER                                     
000025     SELECT W56166                     ASSIGN TO W56167D1.                
000026     SKIP2                                                                
000027*          --- UTFIL TILL KLIENTER                                        
000028     SELECT W56167                     ASSIGN TO W56167D2.                
000029     SKIP2                                                                
000033*          --- UTFIL FÖR LAGERVÄRDERINGSLISTA                             
000034     SELECT W56169                     ASSIGN TO W56167D3.                
000035     EJECT                                                                
000036 DATA DIVISION.                                                           
000037     SKIP2                                                                
000038 FILE SECTION.                                                            
000039                                                                          
000040 FD  W56166                                                               
000041     RECORDING       F                                                    
000042     BLOCK CONTAINS  0.                                                   
000043*01  -COPY WDR801      -L.                                                
000044                                                                          
000045 FD  W56167                                                               
000046     RECORDING       F                                                    
000047     BLOCK CONTAINS  0.                                                   
000048 01  RATT-POST.                                                           
000049*    03   -COPY WDR801    -L.                                             
000050     03 FILLER                   PIC X(6).                                
000051                                                                          
000058 FD  W56169                                                               
000059     RECORDING       F                                                    
000060     BLOCK CONTAINS  0.                                                   
000061 01  UT-POST2.                                                            
000062*    03   -COPY W56169    -L.                                             
000063     EJECT                                                                
000064 WORKING-STORAGE SECTION.                                                 
000065                                                                          
000066 77  IDPGM                       PIC X(8)    VALUE 'W5616700'.            
000067 77  JA                          PIC X       VALUE 'J'.                   
000068 77  NEJ                         PIC X       VALUE 'N'.                   
000069                                                                          
000070 01  W-KLIENT                    PIC X       VALUE 'N'.                   
000071 01  SPAR-IDSYSMOT               PIC X(6)    VALUE SPACE.                 
000072                                                                          
000073                                                                          
000074 01  POST-SW                     PIC X      VALUE 'J'.                    
000075     88 POST-OK                             VALUE 'J'.                    
000076     88 POST-FEL                            VALUE 'N'.                    
000077                                                                          
000078 77  W56166-EOF-SW               PIC X       VALUE 'N'.                   
000079     88  END-OF-W56166                       VALUE 'J'.                   
000080     EJECT                                                                
000081                                                                          
000082 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
000083*01  -COPY WWIDFTG                                                        
000084     EJECT                                                                
000085                                                                          
000086 01  DYNAMISKA-SUBPROGRAM.                                                
000087*                                                                         
000088     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000089     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000090     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000091     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000092     SKIP2                                                                
000093*    --- PARAMETRAR TILL ABEND                                            
000094                                                                          
000095 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000096 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000097 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000098     SKIP2                                                                
000099 01  FELTEXT.                                                             
000100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000101     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000102     EJECT                                                                
000103*    --- PARAMETRAR TILL POSTSUM                                          
000104*                                                                         
000105*01  -COPY W0005   -PRE  POSTSUM-                                         
000106     EJECT                                                                
000107 01  IN-AREA-START               PIC X(24)   VALUE                        
000108                                 'IN-AREA-START  '.                       
000109     SKIP2                                                                
000110                                                                          
000111*01  AREA -COPY WDR801     -PRE IN-                                       
000112*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR801-DATA               
000113     EJECT                                                                
000114 01  RATT-AREA-START             PIC X(24)   VALUE                        
000115                                 'RATT-AREA-START  '.                     
000116     SKIP2                                                                
000117                                                                          
000118*01  AREA -COPY WDR801     -PRE RATT-                                     
000119*    05   -COPY W510EKHA   -PRE RATT- -RED RATT-FIL-WDR801-DATA           
000120     05   RATT-EKH-IDSYSMOT      PIC X(6).                                
000121     EJECT                                                                
000126 01  DIR-AREA-START             PIC X(24)   VALUE                         
000127                                'DIR-AREA START '.                        
000128*01   -COPY W56169                                                        
000129     EJECT                                                                
000130*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000131*                                                                         
000132 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000133     SKIP3                                                                
000134 01  NYCKLAR-TILL-DLI.                                                    
000135     03  W-WDH501KY-X.                                                    
000136         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
000137         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
000138     03  W-KDEKSHT-X.                                                     
000139         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
000140     03  W-KDEKNIVA-X.                                                    
000141         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
000142     03  W-IDSYSMOT-X.                                                    
000143         05  W-IDSYSMOT          PIC X(6)        VALUE SPACE.             
000144     SKIP2                                                                
000145*    --- STATUS-KOD FRÅN IMS                                              
000146 01  STATUS-WS                   PIC XX.                                  
000147     88  SEGMENT-FINNS                       VALUE '  '.                  
000148     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000149     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000150     SKIP2                                                                
000151 01  GODK-STATUSKODER.                                                    
000152     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000153     SKIP3                                                                
000154 01  SSA1                        PIC X(64).                               
000155 01  SSA2                        PIC X(64).                               
000156 01  SSA3                        PIC X(64).                               
000157     EJECT                                                                
000158*    --- IMS FUNKTIONSKODER                                               
000159*01  -COPY W0003                                                          
000160     EJECT                                                                
000161*    ---  DLI INPUT-OUTPUT AREA                                           
000162 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
000163 01  DLI-IO-WDH501.                                                       
000164*    03  -COPY WDH501                                                     
000165     EJECT                                                                
000166 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
000167 01  DLI-IO-WDH511.                                                       
000168*    03  -COPY WDH511                                                     
000169     EJECT                                                                
000170 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
000171 01  DLI-IO-WDH521.                                                       
000172*    03  -COPY WDH521                                                     
000173     EJECT                                                                
000174 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
000175 01  DLI-IO-WDH531.                                                       
000176*    03  -COPY WDH531                                                     
000177     EJECT                                                                
000178 LINKAGE SECTION.                                                         
000179                                                                          
000180                                                                          
000181*01  -COPY W0008  -PRE WDH5-                                              
000182     05  FILLER                  PIC X.                                   
000183     EJECT                                                                
000184 PROCEDURE DIVISION USING WDH5-PCB .                                      
000185                                                                          
000186 MAIN SECTION.                                                            
000187     ENTRY 'DLITCBL' USING WDH5-PCB.                                      
000188                                                                          
000189                                                                          
000190     PERFORM A-INIT                                                       
000191                                                                          
000192     PERFORM S01-LAES-W56166                                              
000193     PERFORM UNTIL END-OF-W56166                                          
000194       PERFORM B-FLYTTA-DATA                                              
000195       PERFORM C-MATCHA-POST                                              
000196       PERFORM S01-LAES-W56166                                            
000197     END-PERFORM                                                          
000198                                                                          
000199                                                                          
000200     PERFORM Z-FINIT                                                      
000201                                                                          
000202     MOVE ZERO TO RETURN-CODE                                             
000203     GOBACK                                                               
000204     .                                                                    
000205     EJECT                                                                
000206 A-INIT SECTION.                                                          
000207                                                                          
000208     OPEN INPUT  W56166                                                   
000209                                                                          
000210     OPEN OUTPUT W56167                                                   
000212                 W56169                                                   
000213                                                                          
000214     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000215     .                                                                    
000216     EJECT                                                                
000217                                                                          
000218 B-FLYTTA-DATA SECTION.                                                   
000219     MOVE WC-IDFTG-US        TO W-IDFTG                                   
000220     MOVE IN-EKH-KDEKHHT     TO W-KDEKHHT                                 
000221     MOVE IN-EKH-KDEKSHT     TO W-KDEKSHT                                 
000222     MOVE IN-EKH-KDEKNIVA    TO W-KDEKNIVA                                
000223     .                                                                    
000224     EJECT                                                                
000225 C-MATCHA-POST SECTION.                                                   
000226                                                                          
000227     MOVE 'NEJ' TO W-KLIENT                                               
000228     MOVE SPACE  TO SPAR-IDSYSMOT                                         
000229     PERFORM IMS-GET-WDH5-ALL                                             
000230     IF SEGMENT-FINNS                                                     
000231       PERFORM IMS-GNP-WDH531                                             
000232       IF SEGMENT-FINNS                                                   
000233         PERFORM UNTIL SEGMENT-SAKNAS                                     
000234         IF SYST-IDSYSMOT NOT  = SPACE                                    
000235           MOVE SYST-IDSYSMOT TO W-IDSYSMOT                               
000236           IF W-IDSYSMOT NOT   = SPAR-IDSYSMOT                            
000237             MOVE 'JA'        TO W-KLIENT                                 
000238             PERFORM D-SKAPA-RATTPOST                                     
000239             PERFORM F-SKAPA-DIR-POST                                     
000243             MOVE W-IDSYSMOT TO SPAR-IDSYSMOT                             
000244           END-IF                                                         
000245         END-IF                                                           
000246         PERFORM IMS-GNP-WDH531                                           
000247         END-PERFORM                                                      
000248       ELSE                                                               
000249         IF W-KDEKNIVA = 'MOMS' OR 'SUM'                                  
000250           PERFORM D-SKAPA-RATTPOST                                       
000251         END-IF                                                           
000252       END-IF                                                             
000253     END-IF                                                               
000254     .                                                                    
000255     EJECT                                                                
000256 D-SKAPA-RATTPOST SECTION.                                                
000257                                                                          
000258     MOVE IN-AREA        TO RATT-AREA                                     
000259     MOVE W-IDSYSMOT     TO RATT-EKH-IDSYSMOT                             
000260                                                                          
000261     PERFORM S11-SKRIV-W56167                                             
000262     .                                                                    
000263     EJECT                                                                
000293 F-SKAPA-DIR-POST SECTION.                                                
000294                                                                          
000295                                                                          
000296     IF IN-EKH-FLLSBOK = 'N' AND IN-EKH-KDEKNIVA = 'DET' AND              
000297        ((IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '301') OR           
000298         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '310') OR           
000299         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '311') OR           
000300         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '314') OR           
000301         (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '351'))             
000302       MOVE IN-EKH-DAVERDAT  TO DIR-DAVERDAT                              
000303       MOVE IN-EKH-IDARTNR   TO DIR-IDARTNR                               
000304       MOVE IN-EKH-IDDC-SEND TO DIR-IDDC                                  
000305       MOVE IN-EKH-IDDISTR   TO DIR-IDDISTR                               
000306       MOVE IN-EKH-IDKUNDNR  TO DIR-IDKUNDNR                              
000307       MOVE IN-EKH-IDVERGL   TO DIR-IDVERGL                               
000308       MOVE IN-EKH-KDEKHHT   TO DIR-KDEKHHT                               
000309       MOVE IN-EKH-KDEKSHT   TO DIR-KDEKSHT                               
000310       MOVE IN-EKH-KDPRODSL  TO DIR-KDPRODSL                              
000311       MOVE IN-EKH-KVANTAL   TO DIR-KVANTAL                               
000312       MOVE IN-EKH-PRARTSTD  TO DIR-PRARTSTD                              
000313       MOVE IN-EKH-PRLANDCO  TO DIR-PRLANDCO                              
000314       MOVE IN-EKH-IDORDNR5  TO DIR-IDORDNR5                              
000315       PERFORM S13-SKRIV-W56169                                           
000316     END-IF                                                               
000317     .                                                                    
000318     EJECT                                                                
000319 Z-FINIT SECTION.                                                         
000320                                                                          
000321     CLOSE W56166                                                         
000322           W56167                                                         
000324           W56169                                                         
000325                                                                          
000326     MOVE 'S' TO POSTSUM-OPKOD                                            
000327     CALL POSTSUM USING POSTSUM-PARM                                      
000328     .                                                                    
000329     EJECT                                                                
000330 S01-LAES-W56166  SECTION.                                                
000331                                                                          
000332     READ W56166 INTO IN-AREA                                             
000333     AT END                                                               
000334        MOVE HIGH-VALUE TO IN-AREA                                        
000335        SET END-OF-W56166 TO TRUE                                         
000336                                                                          
000337     NOT AT END                                                           
000338        MOVE 'W56166'   TO POSTSUM-FDNAMN                                 
000339        MOVE 'W56167D1' TO POSTSUM-DDNAMN2                                
000340        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
000341        CALL POSTSUM USING POSTSUM-PARM                                   
000342     END-READ                                                             
000343     .                                                                    
000344     EJECT                                                                
000345 S11-SKRIV-W56167 SECTION.                                                
000346                                                                          
000347     WRITE RATT-POST FROM RATT-AREA                                       
000348                                                                          
000349     MOVE 'RATT'     TO POSTSUM-TRANSTYP                                  
000350     MOVE 'W56167'   TO POSTSUM-FDNAMN                                    
000351     MOVE 'W56167D2' TO POSTSUM-DDNAMN2                                   
000352     CALL POSTSUM USING POSTSUM-PARM                                      
000353     .                                                                    
000354     EJECT                                                                
000365 S13-SKRIV-W56169 SECTION.                                                
000366                                                                          
000367     WRITE UT-POST2 FROM DIR-W56169                                       
000368                                                                          
000369     MOVE 'DIR '     TO POSTSUM-TRANSTYP                                  
000370     MOVE 'W56169'   TO POSTSUM-FDNAMN                                    
000371     MOVE 'W56167D3' TO POSTSUM-DDNAMN2                                   
000372     CALL POSTSUM USING POSTSUM-PARM                                      
000373     .                                                                    
000374     EJECT                                                                
000375* --- IMS SEKTIONER ---                                                   
000376                                                                          
000377 IMS-GET-WDH5-ALL SECTION.                                                
000378                                                                          
000379     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
000380          DELIMITED BY SIZE INTO SSA1                                     
000381     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
000382          DELIMITED BY SIZE INTO SSA2                                     
000383     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
000384          DELIMITED BY SIZE INTO SSA3                                     
000385     MOVE '  GE' TO GODK-STATUSKODER                                      
000386     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2 SSA3          
000387     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
000388     PERFORM IMS-STATUSKONTROLL                                           
000389     .                                                                    
000390     EJECT                                                                
000391 IMS-GNP-WDH531   SECTION.                                                
000392                                                                          
000393     STRING 'WDH531   '                                                   
000394          DELIMITED BY SIZE INTO SSA1                                     
000395     MOVE '  GE' TO GODK-STATUSKODER                                      
000396     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
000397     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
000398     PERFORM IMS-STATUSKONTROLL                                           
000399     .                                                                    
000400     EJECT                                                                
000401                                                                          
000402 IMS-STATUSKONTROLL SECTION.                                              
000403                                                                          
000404     SET STATUS-IX TO 1                                                   
000405     SEARCH GODK-STATUS                                                   
000406       AT END                                                             
000407         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000408           DELIMITED BY SIZE INTO FELTEXT                                 
000409         DISPLAY FELTEXT                                                  
000410         CALL FELLOG                                                      
000411       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000412         CONTINUE                                                         
000413     END-SEARCH                                                           
000414     .                                                                    
