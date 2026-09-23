000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W6114000.                                                
000004*AUTHOR.         LARS THELL.                                              
000005*DATE-WRITTEN.   92/07/01.                                                
000006                                                                          
000007*    REMARKS.                                                             
000008*                                                                         
000009*    FUNKTION:                                                            
000010*        SB SOM LÄSER NED W6G3 OCH SKAPAR FILER MED INNEHÅLL              
000011*        EFTER BEHOV.                                                     
000012*                                                                         
000013*        PROGRAMMET LÄSER      W6FILA (W6G3)                              
000014*                                                                         
000015*                                                                         
000016*    CHANGES:                                                             
000017*        MARCH-2008.  ETRACKER 6442199.                                   
000018*        FILE W61140B EXTENDED WITH CLAG-VLARTNTO.                        
000019*        THIS NEW FIELD INITIATED WITH ZERO.       /C.E.                  
000020*                                                                         
000021*                                                                         
000022*    ABENDKODER:                                                          
000023*        U0016 -  . . . .                                                 
000024*        U1000 -  . . . .                                                 
000025*                                                                         
000026                                                                          
000027     SKIP3                                                                
000028 ENVIRONMENT DIVISION.                                                    
000029     SKIP2                                                                
000030 INPUT-OUTPUT SECTION.                                                    
000031                                                                          
000032 FILE-CONTROL.                                                            
000033     SKIP2                                                                
000034*          --- FIL MED INFO FÖR BELÄGGNINGSLISTA                          
000035     SELECT W61140                     ASSIGN TO W61140D1.                
000036*          --- FIL MED INFO FÖR RENSNING                                  
000037     SELECT W61141                     ASSIGN TO W61140D2.                
000038     EJECT                                                                
000039 DATA DIVISION.                                                           
000040     SKIP3                                                                
000041 FILE SECTION.                                                            
000042     SKIP3                                                                
000043 FD  W61140                                                               
000044     RECORDING       F                                                    
000045     BLOCK CONTAINS  0.                                                   
000046     SKIP2                                                                
000047*01  POST -COPY W6114001 -PRE  UT01-  -L.                                 
000048     EJECT                                                                
000049                                                                          
000050 FD  W61141                                                               
000051     RECORDING       F                                                    
000052     BLOCK CONTAINS  0.                                                   
000053     SKIP2                                                                
000054*01  POST -COPY W6114101 -PRE  UT02-  -L.                                 
000055     EJECT                                                                
000056 WORKING-STORAGE SECTION.                                                 
000057     SKIP2                                                                
000058                                                                          
000059*    -- CHECKED BY WY2000                                                 
000060 77  IDPGM                       PIC X(8)    VALUE 'W6114000'.            
000061 77  JA                          PIC X       VALUE 'J'.                   
000062 77  NEJ                         PIC X       VALUE 'N'.                   
000063     EJECT                                                                
000064 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000065 01  FILLER REDEFINES DAGENS-DATUM.                                       
000066     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000067     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000068     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000069     EJECT                                                                
000070*      --- VALID IDDC CODES                                               
000071*                                                                         
000072*01    -COPY WWDCKONS                                                     
000073       EJECT                                                              
000074 01  DYNAMISKA-SUBPROGRAM.                                                
000075*                                                                         
000076     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000077     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000078     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000079     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
000080     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000081     SKIP2                                                                
000082*    --- PARAMETRAR TILL ABEND                                            
000083                                                                          
000084 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000085 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000086     SKIP2                                                                
000087 01  FELTEXT.                                                             
000088     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000089     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000090     EJECT                                                                
000091*    --- PARAMETRAR TILL DATKORT                                          
000092*                                                                         
000093 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61151'.              
000094     SKIP2                                                                
000095 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
000096     SKIP2                                                                
000097*01  -COPY WDATKORT                                                       
000098     EJECT                                                                
000099*    --- PARAMETRAR TILL POSTSUM                                          
000100*                                                                         
000101*01  -COPY W0005   -PRE  POSTSUM-                                         
000102     EJECT                                                                
000103 01  IN-AREA-START                PIC X(24)   VALUE                       
000104                                 'IN-AREA-START  '.                       
000105     SKIP2                                                                
000106                                                                          
000107*01  -COPY W6010011 -PRE OLD-.                                            
000108     EJECT                                                                
000109*01  -COPY W6010012.                                                      
000110     EJECT                                                                
000111 01  UT-AREA-START                PIC X(24)   VALUE                       
000112                                 'UT-AREA-START  '.                       
000113     SKIP2                                                                
000114                                                                          
000115*01  AREA -COPY W6114001     -PRE UT01-                                   
000116     EJECT                                                                
000117                                                                          
000118*01  AREA -COPY W6114101     -PRE UT02-                                   
000119     EJECT                                                                
000120                                                                          
000121*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000122*                                                                         
000123     EJECT                                                                
000124 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000125     SKIP3                                                                
000126*    --- STATUS-KOD FRÅN IMS                                              
000127 01  STATUS-WS                   PIC XX.                                  
000128     88  SEGMENT-FINNS                       VALUE '  '.                  
000129     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
000130     SKIP2                                                                
000131 01  GODK-STATUSKODER.                                                    
000132     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000133     SKIP3                                                                
000134 01  SSA1                        PIC X(64).                               
000135     EJECT                                                                
000136*    --- IMS FUNKTIONSKODER                                               
000137*01  -COPY W0003                                                          
000138     EJECT                                                                
000139*    ---  DLI INPUT-OUTPUT AREA                                           
000140 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000141     SKIP3                                                                
000142 01  DLI-IO-AREA.                                                         
000143     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
000144     SKIP3                                                                
000145     03  W6FILA01 REDEFINES IO-AREA.                                      
000146*        05  -COPY W6G301                                                 
000147     EJECT                                                                
000148 LINKAGE SECTION.                                                         
000149                                                                          
000150*01  -COPY W0008  -PRE FILA-                                              
000151     05  FILLER                  PIC X.                                   
000152     EJECT                                                                
000153 PROCEDURE DIVISION  USING FILA-PCB.                                      
000154     ENTRY 'DLITCBL' USING FILA-PCB.                                      
000155                                                                          
000156     PERFORM A-INIT                                                       
000157     PERFORM IMS-GN-FILA-FILA01                                           
000158     PERFORM UNTIL SEGMENT-SAKNAS                                         
000159         IF FIL-IDCPYTXT           = 'W6010011' OR                        
000160            FIL-IDCPYTXT           = 'W6010012'                           
000161             IF FIL-IDCPYTXT         = 'W6010012'                         
000162               MOVE FIL-W6G301-DATA  TO W601-W6010012                     
000163             ELSE                                                         
000164               MOVE FIL-W6G301-DATA  TO OLD-W601-W6010011                 
000165               PERFORM S01-FLYTTA-TILL-NYA-CPY                            
000166             END-IF                                                       
000167             PERFORM B-SKAPA-GENOMLOPPSTID-FIL                            
000168             PERFORM C-SKAPA-RENSNING-FIL                                 
000169         END-IF                                                           
000170         PERFORM IMS-GN-FILA-FILA01                                       
000171     END-PERFORM                                                          
000172                                                                          
000173     PERFORM Z-FINIT                                                      
000174                                                                          
000175     MOVE ZERO TO RETURN-CODE                                             
000176     GOBACK                                                               
000177     .                                                                    
000178     EJECT                                                                
000179 A-INIT SECTION.                                                          
000180                                                                          
000181     OPEN OUTPUT W61140                                                   
000182                 W61141                                                   
000183     SKIP2                                                                
000184     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000185     MOVE D-AAR                TO  DAGENS-DATUM-AAR                       
000186     MOVE D-MAANAD             TO  DAGENS-DATUM-MAANAD                    
000187     MOVE D-DAG                TO  DAGENS-DATUM-DAG                       
000188     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
000189     .                                                                    
000190     EJECT                                                                
000191 B-SKAPA-GENOMLOPPSTID-FIL  SECTION.                                      
000192                                                                          
000193     MOVE FIL-TIREGDAT         TO UT01-TIREGDAT                           
000194     MOVE FIL-TIKLOCK          TO UT01-TIKLOCK                            
000195     MOVE W601-IDDC            TO UT01-IDDC                               
000196     MOVE W601-IDLOPNRM        TO UT01-IDLOPNRM                           
000197     MOVE W601-IDRADNR         TO UT01-IDRADNR                            
000198     MOVE W601-KDINLPRIO       TO UT01-KDINLPRIO                          
000199     MOVE W601-KDINLSTA        TO UT01-KDINLSTA                           
000200     MOVE W601-KDINLUPF        TO UT01-KDINLUPF                           
000201     MOVE W601-KDINLUPF-NXT    TO UT01-KDINLUPF-NXT                       
000202*                                                                         
000203     IF W601-KVINLART NUMERIC                                             
000204       MOVE W601-KVINLART      TO UT01-KVINLART                           
000205     ELSE                                                                 
000206       MOVE +0                 TO UT01-KVINLART                           
000207     END-IF                                                               
000208*                                                                         
000209     MOVE W601-PRARTSTD        TO UT01-PRARTSTD                           
000210*                                                                         
000211     IF W601-KVKOLLI NUMERIC                                              
000212       MOVE W601-KVKOLLI       TO UT01-KVKOLLI                            
000213     ELSE                                                                 
000214       MOVE +0                 TO UT01-KVKOLLI                            
000215     END-IF                                                               
000216*                                                                         
000217     IF W601-FLINLI = NEJ OR JA                                           
000218       MOVE W601-FLINLI        TO UT01-FLINLI                             
000219     ELSE                                                                 
000220       MOVE NEJ                TO UT01-FLINLI                             
000221     END-IF                                                               
000222                                                                          
000223     MOVE +0                   TO UT01-VLARTNTO                           
000224*                                                                         
000225     PERFORM S11-SKRIV-W61140                                             
000226     .                                                                    
000227     EJECT                                                                
000228 C-SKAPA-RENSNING-FIL      SECTION.                                       
000229                                                                          
000230     MOVE FIL-IDPGM            TO UT02-IDPGM                              
000231     MOVE FIL-TIREGDAT         TO UT02-TIREGDAT                           
000232     MOVE FIL-TIKLOCK          TO UT02-TIKLOCK                            
000233     MOVE FIL-IDSEKVNR         TO UT02-IDSEKVNR                           
000234     MOVE FIL-IDCPYTXT         TO UT02-IDCPYTXT                           
000235     MOVE W601-IDDC            TO UT02-IDDC                               
000236     MOVE W601-IDLOPNRM        TO UT02-IDLOPNRM                           
000237     MOVE W601-IDRADNR         TO UT02-IDRADNR                            
000238     MOVE W601-KDINLPRIO       TO UT02-KDINLPRIO                          
000239     MOVE W601-KDINLSTA        TO UT02-KDINLSTA                           
000240     MOVE W601-KDINLUPF        TO UT02-KDINLUPF                           
000241     MOVE W601-KDINLUPF-NXT    TO UT02-KDINLUPF-NXT                       
000242                                                                          
000243     IF W601-KVINLART NUMERIC                                             
000244       MOVE W601-KVINLART      TO UT02-KVINLART                           
000245     ELSE                                                                 
000246       MOVE +0                 TO UT02-KVINLART                           
000247     END-IF                                                               
000248                                                                          
000249     IF W601-KVKOLLI  NUMERIC                                             
000250       MOVE W601-KVKOLLI       TO UT02-KVKOLLI                            
000251     ELSE                                                                 
000252       MOVE +0                 TO UT02-KVKOLLI                            
000253     END-IF                                                               
000254                                                                          
000255     IF W601-FLINLI = NEJ OR JA                                           
000256       MOVE W601-FLINLI        TO UT02-FLINLI                             
000257     ELSE                                                                 
000258       MOVE NEJ                TO UT02-FLINLI                             
000259     END-IF                                                               
000260                                                                          
000261     MOVE W601-PRARTSTD        TO UT02-PRARTSTD                           
000262     PERFORM S12-SKRIV-W61141                                             
000263     .                                                                    
000264     EJECT                                                                
000265 Z-FINIT SECTION.                                                         
000266     CLOSE W61140                                                         
000267           W61141                                                         
000268     SKIP2                                                                
000269     MOVE 'S'                  TO POSTSUM-OPKOD                           
000270     CALL POSTSUM USING POSTSUM-PARM                                      
000271     .                                                                    
000272     EJECT                                                                
000273 S01-FLYTTA-TILL-NYA-CPY SECTION.                                         
000274                                                                          
000275     MOVE WC-CDC-SE            TO W601-IDDC                               
000276     MOVE OLD-W601-IDLOPNRM    TO W601-IDLOPNRM                           
000277     MOVE OLD-W601-IDRADNR     TO W601-IDRADNR                            
000278     MOVE OLD-W601-KDINLPRIO   TO W601-KDINLPRIO                          
000279     MOVE OLD-W601-KDINLSTA    TO W601-KDINLSTA                           
000280     MOVE OLD-W601-KDINLUPF    TO W601-KDINLUPF                           
000281     MOVE OLD-W601-KDINLUPF-NXT TO W601-KDINLUPF-NXT                      
000282     MOVE OLD-W601-KVINLART    TO W601-KVINLART                           
000283     MOVE OLD-W601-PRARTSTD    TO W601-PRARTSTD                           
000284     MOVE OLD-W601-KVKOLLI     TO W601-KVKOLLI                            
000285     MOVE OLD-W601-FLINLI      TO W601-FLINLI                             
000286     .                                                                    
000287     EJECT                                                                
000288 S11-SKRIV-W61140 SECTION.                                                
000289     SKIP2                                                                
000290     WRITE UT01-POST FROM UT01-AREA                                       
000291                                                                          
000292     MOVE 'W61140'             TO POSTSUM-FDNAMN                          
000293     MOVE 'W61140D1'           TO POSTSUM-DDNAMN2                         
000294     CALL POSTSUM USING POSTSUM-PARM                                      
000295     .                                                                    
000296     EJECT                                                                
000297 S12-SKRIV-W61141 SECTION.                                                
000298     SKIP2                                                                
000299     WRITE UT02-POST FROM UT02-AREA                                       
000300                                                                          
000301     MOVE 'W61141'             TO POSTSUM-FDNAMN                          
000302     MOVE 'W61140D2'           TO POSTSUM-DDNAMN2                         
000303     CALL POSTSUM USING POSTSUM-PARM                                      
000304     .                                                                    
000305     EJECT                                                                
000306* --- IMS SEKTIONER ---                                                   
000307     SKIP3                                                                
000308 IMS-GN-FILA-FILA01   SECTION.                                            
000309     SKIP2                                                                
000310     CALL CBLTDLI USING GN FILA-PCB DLI-IO-AREA                           
000311     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
000312     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000313     PERFORM IMS-STATUSKONTROLL                                           
000314     .                                                                    
000315     EJECT                                                                
000316 IMS-STATUSKONTROLL SECTION.                                              
000317     SKIP2                                                                
000318     SET STATUS-IX TO 1                                                   
000319     SEARCH GODK-STATUS                                                   
000320       AT END                                                             
000321         MOVE 'FELAKTIG STATUS-CODE ' TO FELTEXT-STR                      
000322         DISPLAY FELTEXT                                                  
000323         CALL FELLOG                                                      
000324       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000325         CONTINUE                                                         
000326     END-SEARCH                                                           
000327     .                                                                    
