000001*********************************************                             
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W4766900.                                                
000004 AUTHOR.         MOGREN STINA.                                            
000005 DATE-WRITTEN.   02/04/22.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        INGÅR I SOP-RUTIN W476S7                                         
000010*        PROGRAMMET LÄSER W47668 , SORTERADE POSTER                       
000011*        POSTER TILL VIPS W476RIK/RIL/RIM/RIN/RIO/RIP                     
000012*        MED INFO FRÅN WDE4 BL.A.                                         
000013*        LIKA POSTER PLOCKAS BORT                                         
000014*                                                                         
000015*                                                                         
000016                                                                          
000017     SKIP3                                                                
000018 ENVIRONMENT DIVISION.                                                    
000019     SKIP2                                                                
000020 INPUT-OUTPUT SECTION.                                                    
000021                                                                          
000022 FILE-CONTROL.                                                            
000023     SKIP2                                                                
000024*          --- POSTER FRÅN W4766800 SOM SKA DUBLETTKOLLAS                 
000025     SELECT W47668                     ASSIGN TO W47669D1.                
000026     SKIP2                                                                
000027*          --- DUBLETTRENSAD UTFIL                                        
000028     SELECT W47669                     ASSIGN TO W47669D2.                
000029     EJECT                                                                
000030 DATA DIVISION.                                                           
000031     SKIP2                                                                
000032 FILE SECTION.                                                            
000033     SKIP3                                                                
000034 FD  W47668                                                               
000035     RECORDING       F                                                    
000036     BLOCK CONTAINS  0.                                                   
000037                                                                          
000038*01  -COPY W4766601     -L.                                               
000039     SKIP3                                                                
000040 FD  W47669                                                               
000041     RECORDING       V                                                    
000042     BLOCK CONTAINS  0.                                                   
000043*01  POST   -COPY W461RIK1   -PRE RIK-  -L.                               
000044*01  POST   -COPY W461RILN   -PRE RIL-  -L.                               
000045*01  POST   -COPY W461RIM2   -PRE RIM-  -L.                               
000046*01  POST   -COPY W461RINN   -PRE RIN-  -L.                               
000047*01  POST   -COPY W461RIO2   -PRE RIO-  -L.                               
000048*01  POST   -COPY W461RIPN   -PRE RIP-  -L.                               
000049     EJECT                                                                
000050 WORKING-STORAGE SECTION.                                                 
000051                                                                          
000052 77  IDPGM                       PIC X(8)    VALUE 'W4766900'.            
000053 77  JA                          PIC X       VALUE 'J'.                   
000054 77  NEJ                         PIC X       VALUE 'N'.                   
000055                                                                          
000056 77  W47668-EOF-SW               PIC X       VALUE 'N'.                   
000057     88  END-OF-W47668                       VALUE 'J'.                   
000058     EJECT                                                                
000059 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
000060                                                                          
000061 01  DYNAMISKA-SUBPROGRAM.                                                
000062*                                                                         
000063     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000064     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000065     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000066     SKIP2                                                                
000067*    --- PARAMETRAR TILL ABEND                                            
000068                                                                          
000069 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000070 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000071 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000072     SKIP2                                                                
000073*    --NYCKLAR FÖR HOP-SORTERING                                          
000074 01  W-IDPTYP                   PIC X(3)   VALUE SPACE.                   
000075 01  W-IDFAKT                   PIC S9(7)  VALUE ZERO  COMP-3.            
000076 01  W-IDDISTR                  PIC S9(5)  VALUE ZERO  COMP-3.            
000077 01  W-IDKUNDNR                 PIC S9(7)  VALUE ZERO  COMP-3.            
000078 01  W-IDORDER                  PIC S9(7)  VALUE ZERO  COMP-3.            
000079 01  W-IDKUNDNR-S               PIC S9(7)  VALUE ZERO  COMP-3.            
000080 01  W-IDPRODNR                 PIC S9(7)  VALUE ZERO  COMP-3.            
000081 01  W-IDKOLLI                  PIC  9(5)  VALUE ZERO.                    
000082 01  W-IDPURAD                  PIC S9(5)  VALUE ZERO  COMP-3.            
000083                                                                          
000084 01  WX-IDFAKT                  PIC S9(7)  VALUE ZERO  COMP-3.            
000085*    --- PARAMETRAR TILL POSTSUM                                          
000086*                                                                         
000087*01  -COPY W0005   -PRE  POSTSUM-                                         
000088     EJECT                                                                
000089 01  IN-AREA-START               PIC X(16)   VALUE                        
000090                                 'IN-AREA-START  '.                       
000091 01  IN-AREA.                                                             
000092*    03  -COPY W4766601                                                   
000093     EJECT                                                                
000094 01  UT-AREA-START               PIC X(16)   VALUE                        
000095                                 'UT-AREA-START  '.                       
000096 01  FILLER.                                                              
000097 03  UT-AREA                     PIC X(250) VALUE SPACE.                  
000098     SKIP2                                                                
000099*03  -COPY W461RIK1   -RED UT-AREA                                        
000100*                                                                         
000101*03  -COPY W461RILN   -RED UT-AREA                                        
000102*                                                                         
000103*03  -COPY W461RIM2   -RED UT-AREA                                        
000104*                                                                         
000105*03  -COPY W461RINN   -RED UT-AREA                                        
000106*                                                                         
000107*03  -COPY W461RIPN   -RED UT-AREA                                        
000108*                                                                         
000109*03  -COPY W461RIO2   -RED UT-AREA                                        
000110     EJECT                                                                
000111 PROCEDURE DIVISION.                                                      
000112 MAIN SECTION.                                                            
000113                                                                          
000114                                                                          
000115     PERFORM A-INIT                                                       
000116                                                                          
000117     PERFORM S01-LAES-W47668                                              
000118     IF NOT END-OF-W47668                                                 
000119       MOVE RJX-FILLER         TO UT-AREA                                 
000120       PERFORM S11-SKRIV-W47669                                           
000121       PERFORM S10-SPARA-ID                                               
000122     END-IF                                                               
000123     PERFORM UNTIL END-OF-W47668                                          
000124                                                                          
000125                                                                          
000126       PERFORM D-TESTA-SKAPA-POSTER                                       
000127                                                                          
000128                                                                          
000129       PERFORM S01-LAES-W47668                                            
000130     END-PERFORM                                                          
000131                                                                          
000132                                                                          
000133     PERFORM Z-FINIT                                                      
000134                                                                          
000135     MOVE ZERO TO RETURN-CODE                                             
000136     GOBACK                                                               
000137     .                                                                    
000138     EJECT                                                                
000139 A-INIT SECTION.                                                          
000140                                                                          
000141     OPEN INPUT  W47668                                                   
000142     OPEN OUTPUT W47669                                                   
000143                                                                          
000144     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000145     .                                                                    
000146     EJECT                                                                
000147 D-TESTA-SKAPA-POSTER  SECTION.                                           
000148                                                                          
000149     IF W-IDFAKT     = RJX-IDFAKT     AND                                 
000150        W-IDDISTR    = RJX-IDDISTR    AND                                 
000151        W-IDKUNDNR   = RJX-IDKUNDNR   AND                                 
000152        W-IDPRODNR   = RJX-IDPRODNR   AND                                 
000153        W-IDKOLLI    = RJX-IDKOLLI    AND                                 
000154        W-IDORDER    = RJX-IDORDER    AND                                 
000155        W-IDKUNDNR-S = RJX-IDKUNDNR-S AND                                 
000156        W-IDPURAD    = RJX-IDPURAD    AND                                 
000157        W-IDPTYP     = RJX-IDPTYP                                         
000158                                                                          
000159       IF RJX-IDPTYP = 'RIO'                                              
000160         MOVE RJX-FILLER            TO UT-AREA                            
000161         PERFORM S11-SKRIV-W47669                                         
000162       ELSE                                                               
000163         CONTINUE                                                         
000164       END-IF                                                             
000165     ELSE                                                                 
000166        MOVE RJX-FILLER             TO UT-AREA                            
000167        PERFORM S11-SKRIV-W47669                                          
000168        PERFORM S10-SPARA-ID                                              
000169     END-IF                                                               
000170     .                                                                    
000171     EJECT                                                                
000172 Z-FINIT SECTION.                                                         
000173     CLOSE W47668                                                         
000174           W47669                                                         
000175                                                                          
000176     MOVE 'S' TO POSTSUM-OPKOD                                            
000177     CALL POSTSUM USING POSTSUM-PARM                                      
000178     .                                                                    
000179     EJECT                                                                
000180 S01-LAES-W47668  SECTION.                                                
000181     READ W47668 INTO IN-AREA                                             
000182     AT END                                                               
000183        SET END-OF-W47668 TO TRUE                                         
000184                                                                          
000185     NOT AT END                                                           
000186        MOVE 'W47668'       TO POSTSUM-FDNAMN                             
000187        MOVE 'W47669D1'     TO POSTSUM-DDNAMN2                            
000188        MOVE RJX-IDPTYP     TO POSTSUM-TRANSTYP                           
000189        CALL POSTSUM USING POSTSUM-PARM                                   
000190     END-READ                                                             
000191     .                                                                    
000192     EJECT                                                                
000193 S10-SPARA-ID  SECTION.                                                   
000194                                                                          
000195     MOVE RJX-IDFAKT                 TO W-IDFAKT                          
000196     MOVE RJX-IDDISTR                TO W-IDDISTR                         
000197     MOVE RJX-IDKUNDNR               TO W-IDKUNDNR                        
000198     MOVE RJX-IDPRODNR               TO W-IDPRODNR                        
000199     MOVE RJX-IDKOLLI                TO W-IDKOLLI                         
000200     MOVE RJX-IDORDER                TO W-IDORDER                         
000201     MOVE RJX-IDKUNDNR-S             TO W-IDKUNDNR-S                      
000202     MOVE RJX-IDPURAD                TO W-IDPURAD                         
000203     MOVE RJX-IDPTYP                 TO W-IDPTYP                          
000204     .                                                                    
000205     EJECT                                                                
000206 S11-SKRIV-W47669 SECTION.                                                
000207                                                                          
000208     EVALUATE RJX-IDPTYP                                                  
000209       WHEN 'RIK'                                                         
000210         WRITE RIK-POST   FROM RIK-W461RIK1                               
000211         MOVE RJX-IDFAKT  TO WX-IDFAKT                                    
000212       WHEN 'RIL'                                                         
000213         IF RJX-IDFAKT = WX-IDFAKT                                        
000214           WRITE RIL-POST   FROM RIL-W461RILN-CTX                         
000215         END-IF                                                           
000216       WHEN 'RIM'                                                         
000217         WRITE RIM-POST   FROM RIM-W461RIM2-CTX                           
000218       WHEN 'RIN'                                                         
000219         WRITE RIN-POST   FROM RIN-W461RINN-CTX                           
000220       WHEN 'RIO'                                                         
000221         WRITE RIO-POST   FROM RIO-W461RIO2                               
000222       WHEN 'RIP'                                                         
000223         WRITE RIP-POST   FROM RIP-W461RIPN-CTX                           
000224     END-EVALUATE                                                         
000225                                                                          
000226     MOVE RIO-IDPTYP        TO POSTSUM-TRANSTYP                           
000227     MOVE 'W47669'          TO POSTSUM-FDNAMN                             
000228     MOVE 'W47669D2'        TO POSTSUM-DDNAMN2                            
000229     CALL POSTSUM USING POSTSUM-PARM                                      
000230     .                                                                    
000240     EJECT                                                                
