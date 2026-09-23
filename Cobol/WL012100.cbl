       ID DIVISION.                                                             
       PROGRAM-ID.     WL012100.                                                
       AUTHOR.         ANDERSSON BERT.                                          
       DATE-WRITTEN.   04/05/13.                                                
       DATE-COMPILED.                                                           
                                                                                
      *    NAME:       CARPARTS.LDC.CASEREPORTING1                              
      *                                                                         
      *    FUNCTION:                                                            
      *        CASE REPORTING1                                                  
      *        PACK REPORTING OF ORDERPARTS.                                    
      *        WL012100 PROGRAM IS A REPLICA OF W4031300 PROGRAM                
      *        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
      *                                                                         
      *        PACKNINGSRAPPORTERING AV ORDERDELAR.                             
      *        EFTER KONTROLL EN ORDERDEL (=INDATA-RAD PÅ BILDEN) SÅ            
      *        SKER ANROP AV SUBRUTIN W403AVSP DÄR PACKNING OCH                 
      *        AVSLUT AV PACKNINGSRAPPORTERINGEN SKER. ETT ANROP PER            
      *        ORDERDEL GÖRS AV 0129 MOT W403AVSP.                              
      *                                                                         
      *        THE PROGRAM READS     WDK5                                       
      *        THE PROGRAM UPDATES   WDE6                                       
      *        THE PROGRAM UPDATES   WDE4                                       
      *        THE PROGRAM UPDATES   WDQ3                                       
      *                                                                         
      *    INDATA.                                                              
      *        TRANSACTION: WL0121T                                             
      *        REQUEST:     WL0121I1                                            
      *                                                                         
      *    OUTDATA.                                                             
      *        RESPONSE:    WL0121O1                                            
      *                                                                         
      *    CHANGE LOG:                                                          
      *                                                                         
      *      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
      *      ----------------------------------------------------------         
      *      15/07/10 - REDDY RAHUL     - PREVENTATIVE CHANGE TO AVOID          
      *                                   S0C7.                                 
      *                                   E'TRACKER 10260777                    
      *                                                                         
                                                                                
           SKIP3                                                                
       ENVIRONMENT DIVISION.                                                    
           SKIP2                                                                
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
           EJECT                                                                
       DATA DIVISION.                                                           
           SKIP3                                                                
       FILE SECTION.                                                            
           EJECT                                                                
       WORKING-STORAGE SECTION.                                                 
       77  IDPGM                       PIC X(08)   VALUE 'WL012100'.            
                                                                                
      *    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
       77  ERRORTXT                    PIC X(08) VALUE 'ERRORTXT'.              
       77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
       77  KDRC-DISPLAY                PIC Z(5).                                
                                                                                
       77  YES                         PIC X       VALUE 'Y'.                   
       77  NOO                         PIC X       VALUE 'N'.                   
                                                                                
       77  FELTEXT                     PIC X(16).                               
       77  WS-ABSTRACT-ADRESS          PIC X(50)                                
                  VALUE 'CARPARTS.LDC.CASEREPORTING1'.                          
       77    INDX                      PIC S9(9)   VALUE +0   COMP-3.           
       77    RADIX                     PIC S9(9)   VALUE +0   COMP-3.           
       77    RADIND                    PIC S9(9)   VALUE +0   COMP-3.           
       77    RADIND-9                  PIC  9(9)   VALUE  0.                    
       77    RADIND-NUM9               PIC S9(9)   VALUE +0   COMP-3.           
       77    JMF-IND                   PIC S9(9)   VALUE +0   COMP-3.           
       77    MAX-RADINDX               PIC S9(9)   VALUE +15  COMP-3.           
       77    JA                        PIC X       VALUE 'J'.                   
       77    NEJ                       PIC X       VALUE 'N'.                   
       77    SAKNAS                    PIC X       VALUE 'S'.                   
       77    RAETT                     PIC X       VALUE 'R'.                   
       77    FEL-FR-W403AVSP           PIC X       VALUE 'F'.                   
       77    WS-MID-ADRESSFL           PIC XX.                                  
       77    WS-MID-FOLJES             PIC XX.                                  
       77    WS-PRT-KDSVAR-FOLJES      PIC X.                                   
       77    WS-KDMFSFOR               PIC X.                                   
       77    WS-IDDISTR-NUM4           PIC 9(4).                                
       77    WS-IDDISTR-JFR            PIC 9(4).                                
       77    WS-VORD-IDDISTR           PIC 9(5).                                
       77    WS-IDKUNDNR-NUM6          PIC 9(6).                                
       77    WS-RESP-VKORDBTO          PIC S9(6)V9.                             
       77    WS-ORAD-KVLEVART          PIC 9(7).                                
       77    WS-IDORDNR7               PIC 9(7).                                
       77    WS-KVRADER                PIC 9(5).                                
       77    RAD-DISPLAY               PIC 9(9).                                
       77    WS-IDSYSTEM               PIC X(4).                                
                                                                                
       77 WS-ORAD-VLORDNTO             PIC  9(8)   VALUE ZERO.                  
       77 WS-ORAD-VLORDNTO-SUM         PIC  9(8)   VALUE ZERO.                  
       77 WS-KOLLI-VLORDBTO            PIC  9(8)   VALUE ZERO.                  
                                                                                
       77 WS-CURRENT-DATE        PIC 9(8)    VALUE ZERO.                        
       77 WS-CURRENT-TIME        PIC 9(8)    VALUE ZERO.                        
                                                                                
       01  WS-KDMATT                   PIC X.                                   
           88 US-MEASUREMENT           VALUE 'U'.                               
           88 SIS-MEASUREMENT          VALUE 'S'.                               
                                                                                
      *01 -COPY WWDC04                                                          
          SKIP2                                                                 
       01 TEST-IDDISTR                 PIC 9(5)        COMP-3.                  
       01 FILLER REDEFINES TEST-IDDISTR.                                        
      *   03   -COPY WWDIST44.                                                  
          SKIP2                                                                 
                                                                                
       01 ALL-PLUS.                                                             
          03 FILLER                    PIC X(30)  VALUE                         
              '++++++++++++++++++++++++++++++'.                                 
       SKIP3                                                                    
      *                                                                         
      *    --- PARAMETRAR TILL ABEND                                            
       77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
       77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
       77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
           EJECT                                                                
       01  TRANSFER-KUND               PIC 9(7).                                
           88 TRANSFER-KUNDNR          VALUE 0000511                            
                                             0000512                            
                                             0000513.                           
           88  RETUR-KUNDNR            VALUE 0000051.                           
      *                                                                         
       77  INDATA-SW                   PIC X       VALUE 'J'.                   
           88  INDATA-OK                           VALUE 'J'.                   
           88  INDATA-FEL                          VALUE 'N'.                   
                                                                                
       77  KEYS-SW                     PIC X       VALUE 'J'.                   
           88  KEYS-OK                             VALUE 'J'.                   
           88  KEYS-FEL                            VALUE 'N'.                   
                                                                                
       77  ORDERDEL-PACKAD-SW          PIC X       VALUE 'N'.                   
           88  ORDERDEL-PACKAD                     VALUE 'J'.                   
                                                                                
       77  INDATA-FINNS-SW             PIC X       VALUE 'J'.                   
           88  INDATA-FINNS                        VALUE 'J'.                   
           88  INDATA-SAKNAS                       VALUE 'N'.                   
      *                                                                         
       77  DIRLEV-KOLLI-SW             PIC X(01).                               
           88  DIRLEV-KOLLI                        VALUE 'J'.                   
      *                                                                         
       77  WS-IDELMT-ERROR             PIC X(16).                               
       77  WS-IDMSG-ERROR              PIC X(03).                               
       77  WS-IDMSG-INFO               PIC X(03).                               
       77  WS-IDPRODNR-ALPHA           PIC X(7).                                
      *                                                                         
                                                                                
           EJECT                                                                
       01    FILLER                    PIC X(16)   VALUE 'WS-AREA'.             
       01    WS-AREA.                                                           
                                                                                
         03    MAX-ANT-RAD             PIC S9(9)   VALUE +100.                  
         03    WS-ANT-RAD-REST         PIC S9(9)   VALUE ZERO.                  
         03    WS-ANT-RAD-INT          PIC S9(9)   VALUE ZERO.                  
                                                                                
         03    WS-VKORDBTO-RED         PIC 9(5).9  VALUE ZERO.                  
         03    FILLER           REDEFINES WS-VKORDBTO-RED.                      
               05  WS-VKORDBTO-KG      PIC 9(5).                                
               05  WS-VKORDBTO-PUNKT   PIC X.                                   
               05  WS-VKORDBTO-HK      PIC 9.                                   
                                                                                
       01  FILLER                    PIC X(16)   VALUE 'WS-ARB-TAB'.            
       01  WS-ARB-TAB.                                                          
         03  WS-TABSTEG OCCURS 15.                                              
           05  WS-IDDISTR              PIC 9(5).                                
           05  WS-IDKUNDNR             PIC 9(7).                                
           05  WS-IDPRODNR             PIC 9(7).                                
           05  WS-IDORDNR              PIC 9(5)  VALUE 0.                       
           05  WS-IDORDER              PIC 9(7).                                
           05  WS-IDPLKLST             PIC 9(3).                                
           05  WS-IDKOLLI              PIC 9(5).                                
           05  WS-KDKOLLI              PIC X(7).                                
           05  WS-KVORDRAD             PIC S9(5)        COMP-3.                 
           05  WS-KDFRAKT              PIC S9(3)        COMP-3.                 
           05  WS-IDKUNDRF             PIC X(10).                               
           05  WS-KDEMBTYP             PIC 9.                                   
           05  WS-DIKOLLIL             PIC 9(5).                                
           05  WS-DIKOLLIB             PIC 9(3).                                
           05  WS-DIKOLLIH             PIC 9(3).                                
           05  WS-KDKOLLID             PIC X(1).                                
           05  WS-VKTARA               PIC S9(6)V9.                             
           05  WS-VKORDBTO             PIC S9(6)V9.                             
           05  WS-PRTVAL-FOLJES        PIC X.                                   
           05  WS-PRTVAL-ADRESSFL      PIC X.                                   
           05  WS-KORD-VKORDNTO        PIC S9(6)V9.                             
           05  WS-FLAVSP               PIC X.                                   
           05  WS-IDUSER               PIC 9(8).                                
           05  FILLER           REDEFINES WS-IDUSER.                            
               07 FILLER               PIC 9(3).                                
               07 WS-IDANSTNR          PIC 9(5).                                
           EJECT                                                                
      *    --- SUBPROGRAMS AND PARAMETER AREAS                                  
       01  GENERAL-SUBPROGRAMS.                                                 
           03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
           03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
           03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
           03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
           03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
           03  WZ01CALL                PIC X(8)    VALUE 'WZ01CALL'.            
           03  W403AVSP                PIC X(8)    VALUE 'W403AVSP'.            
           03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
           03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
           SKIP3                                                                
      *    --- PARAMETERS TO ABEND                                              
                                                                                
       77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
       77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
       77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
           SKIP3                                                                
       01  MESSAGE-CODES.                                                       
           03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
           EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'W403AVSP '.           
      *01  -COPY W403AVSP                                                       
           EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'WDECAREA '.           
      *01  -COPY WDECAREA                                                       
           EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'WWOMVAND '.           
      *01  -COPY WWOMVAND                                                       
           SKIP2                                                                
       01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
      *01  -COPY WZ01SUB                                                        
           SKIP3                                                                
       01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
      *01  -COPY WZ01SEND                                                       
           SKIP3                                                                
       01  FILLER                      PIC X(08)   VALUE 'WZ01CALL'.            
      *01  -COPY WZ01CALL                                                       
           SKIP3                                                                
       01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
       01  SEND-AREA.                                                           
      *    03  -COPY WZ01REQU -PRE SEND-                                        
      *    03  -COPY WL0121I1 -PRE SEND-                                        
           SKIP3                                                                
       01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
       01  REQU-AREA.                                                           
      *    03  -COPY WZ01REQU                                                   
      *    03  -COPY WL0121I1                                                   
           EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
       01  RESP-AREA.                                                           
      *    03  -COPY WZ01RESP                                                   
      *    03  -COPY WL0121O1                                                   
           EJECT                                                                
       01  MESSAGE-CODES.                                                       
           05  NO-DATA-ENTERED         PIC X(3)   VALUE '014'.                  
           05  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
           05  IS-INVALID              PIC X(3)   VALUE '023'.                  
           05  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
           05  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
           05  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
           05  LINES-NOT-FOUND         PIC X(3)   VALUE '027'.                  
           05  DATA-ENTER-BUT-NO-PRESSED                                        
                                       PIC X(3)   VALUE '013'.                  
           05  WRONG-DC                PIC X(3)   VALUE '023'.                  
       01  MESSAGE-CODES.                                                       
           03  ERR-NO-DATA-ENTERED     PIC X(3)    VALUE '014'.                 
           03  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
           03  ERR-ORDER-PARTS-MISSING PIC X(3)    VALUE '041'.                 
           03  ERR-WRONG-STATUS        PIC X(3)    VALUE '273'.                 
           03  ERR-NO-DANGEROUS-CARGO  PIC X(3)    VALUE '119'.                 
           03  ERR-DISTRICT-NOT-APPROV PIC X(3)    VALUE '120'.                 
           03  ERR-UPDATED             PIC X(3)    VALUE '001'.                 
           03  ERR-ORDER-NOT-PACKED    PIC X(3)    VALUE '021'.                 
           03  ERR-ORDER-REGISTRED     PIC X(3)    VALUE '030'.                 
           03  ERR-ORDER-PART-READY    PIC X(3)    VALUE '116'.                 
           03  ERR-CASE-REPORT-STARTED PIC X(3)    VALUE '113'.                 
           03  ERR-CASE-ALREADY-REPORT PIC X(3)    VALUE '030'.                 
           03  ERR-ERROR-IN-ADDRESS    PIC X(3)    VALUE '123'.                 
           03  ERR-WRONG-LINES         PIC X(3)    VALUE '027'.                 
           03  ERR-USESCREEN-L0197-ORL0199 PIC  X(03)  VALUE '192'.             
           03  MUST-BE-NUMERIC         PIC X(3)    VALUE '024'.                 
           03  MUST-BE-ENTERED         PIC X(3)    VALUE '026'.                 
           03  NOT-FOUND               PIC X(3)    VALUE '027'.                 
           03  DUPLICATE-LINES         PIC X(3)    VALUE '029'.                 
           03  ERR-LINE-ALREADY-ZEROED PIC X(3)    VALUE '118'.                 
           03  ERR-MIXED-CASE-ON-OTHER-TRANS  PIC X(3) VALUE '121'.             
           03  ERR-MIXED-CASE-HAS-NO-TRANS    PIC X(3) VALUE '122'.             
           03  NO-REPORTING-FOR-DIRECT-SUPPLI PIC X(3) VALUE '114'.             
           03  ERR-MORE-CASE-INFO-NEEDED PIC  X(03)  VALUE '140'.               
           03  ERR-ORDER-HAS-WRONG-STATUS     PIC X(03) VALUE '273'.            
           EJECT                                                                
                                                                                
           EJECT                                                                
       01    FILLER                    PIC X(16)                                
                                       VALUE 'KEYS-TILL-DLI'.                   
       01    KEYS-TILL-DLI.                                                     
                                                                                
         03    W-KDKOLLI-WDK5          PIC X(8)    VALUE SPACE.                 
                                                                                
         03    W-IDPRODNR-X.                                                    
           05    W-IDPRODNR            PIC S9(7)   VALUE ZERO  COMP-3.          
                                                                                
         03    W-IDKOLLI-X.                                                     
           05    W-IDKOLLI             PIC S9(5)   VALUE ZERO  COMP-3.          
                                                                                
         03    W-WDQ3D-X.                                                       
           05    W-IDPRODNR-WDQ3D      PIC S9(7)   VALUE ZERO  COMP-3.          
           05    W-IDPLKLST-WDQ3D      PIC S9(3)   VALUE ZERO  COMP-3.          
      *                                                                         
         03    W-WDE401-KUNDORDER-X.                                            
           05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
           05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
           05    W-401-IDKUNDRF.                                                
             07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
             07  FILLER                PIC X(05)   VALUE SPACE.                 
           05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
           05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
      *                                                                         
         03    W-WDE411-IDPURAD-X.                                              
           05    W-420-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
      *                                                                         
         03    W-WDE421-KKOLLI-X.                                               
           05    W-421-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
           05    W-421-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
      *                                                                         
         03    W-IDDC-B6-X.                                                     
           05    W-IDDC-B6             PIC X(2).                                
                                                                                
      ******************************************************************        
      *                                                                         
      *        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
      *                                                                         
       01    IMS-WS.                                                            
         03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
           SKIP3                                                                
      *                        **** STATUS-KOD FRÅN IMS                         
         03    STATUS-WS               PIC XX.                                  
           88    SEGMENT-FINNS                     VALUE '  '.                  
           88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
           SKIP3                                                                
         03    GODK-STATUSKODER.                                                
           05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
           SKIP3                                                                
       01    SSA1                      PIC X(64).                               
       01    SSA2                      PIC X(64).                               
       01    SSA3                      PIC X(64).                               
           EJECT                                                                
      *                            IMS FUNKTIONSKODER                           
      *01    -COPY W0003                                                        
      *                                                                         
           EJECT                                                                
      *                            DLI INPUT-OUTPUT AREA                        
       01    FILLER                    PIC X(16) VALUE 'WDK501-AREA'.           
       01    WLEMBB01 -COPY WDK501                                              
           EJECT                                                                
       01    FILLER                    PIC X(16) VALUE 'WDE601-AREA'.           
       01    WDE601 -COPY WDE601                                                
           EJECT                                                                
       01    FILLER                    PIC X(16) VALUE 'WDE611-AREA'.           
       01    WDE611   -COPY WDE611                                              
           EJECT                                                                
       01    FILLER                    PIC X(16) VALUE 'WDE401-AREA'.           
       01    WDE401 -COPY WDE401                                                
           EJECT                                                                
       01    FILLER                    PIC X(16) VALUE 'WDE411-AREA'.           
       01    WDE411 -COPY WDE411                                                
           EJECT                                                                
       01    FILLER                    PIC X(16) VALUE 'WDE421-AREA'.           
       01    WDE421 -COPY WDE421                                                
           EJECT                                                                
       01    FILLER                    PIC X(16) VALUE 'WDQ301-AREA'.           
       01    WLORQA01 -COPY WDQ301                                              
                                                                                
       LINKAGE SECTION.                                                         
      *01  -COPY W0009  -PRE MSG-                                               
           EJECT                                                                
      *01    -COPY W0008     -PRE DETTAPGM-                                     
           05  FILLER                  PIC X.                                   
           EJECT                                                                
       01  AVSP-4342-PCB               PIC X.                                   
       01  AVSP-2191-PCB               PIC X.                                   
       01 -COPY W0008  -PRE AVSP-4349-                                          
              05 FILLER                PIC X.                                   
           EJECT                                                                
       01  TMS-CRE-PCB                 PIC X.                                   
       01  TMS-DEL-PCB                 PIC X.                                   
       01  ATAB-PCB                    PIC X.                                   
           EJECT                                                                
      *01    -COPY W0008     -PRE EMBB-                                         
              05 FILLER                PIC X.                                   
           EJECT                                                                
      *01    -COPY W0008     -PRE WDE6-                                         
              05 FILLER                PIC X.                                   
           EJECT                                                                
      *01    -COPY W0008     -PRE ORQA-                                         
              05 FILLER                PIC X.                                   
           EJECT                                                                
      *01    -COPY W0008     -PRE WDE4-                                         
              05 FILLER                PIC X.                                   
           EJECT                                                                
      *****AVSP-PCB'N                                                           
       01  AVSP-USEA-PCB               PIC X.                                   
       01  AVSP-WDE41-PCB              PIC X.                                   
       01  AVSP-WDE42-PCB              PIC X.                                   
       01  AVSP-WDE4-PCB               PIC X.                                   
       01  AVSP-WDE4B-PCB              PIC X.                                   
       01  AVSP-WDE4E-PCB              PIC X.                                   
       01  AVSP-WDE6-PCB               PIC X.                                   
       01  AVSP-XXDV-PCB               PIC X.                                   
       01  AVSP-ORQA-PCB               PIC X.                                   
       01  AVSP-XXKW-PCB               PIC X.                                   
       01  AVSP-XXLB-PCB               PIC X.                                   
       01  AVSP-XXJK-PCB               PIC X.                                   
       01  AVSP-ZZAC-PCB               PIC X.                                   
       01  AVSP-ORQI-PCB               PIC X.                                   
       01  AVSP-ORQL-PCB               PIC X.                                   
       01  AVSP-WDE6E-PCB              PIC X.                                   
       01  AVSP-WDE62-PCB              PIC X.                                   
       01  AVSP-ORQICSQ-PCB            PIC X.                                   
       01  AVSP-ORQM-PCB               PIC X.                                   
       01  AVSP-WDE4A-PCB              PIC X.                                   
       01  AVSP-WDM2-PCB               PIC X.                                   
       01  AVSP-ORQA2-PCB              PIC X.                                   
       01  AVSP-ORDP1-PCB              PIC X.                                   
       01  AVSP-ORDP2-PCB              PIC X.                                   
       01  AVSP-XXJN-PCB               PIC X.                                   
       01  AVSP-XXKH-PCB               PIC X.                                   
       01  AVSP-ARTC-PCB               PIC X.                                   
       01  AVSP-WDK7-PCB               PIC X.                                   
       01  AVSP-ARTM-PCB               PIC X.                                   
       01  AVSP-AUTF-PCB               PIC X.                                   
       01  AVSP-4487-PCB               PIC X.                                   
       01  AVSP-4541-PCB               PIC X.                                   
       01  AVSP-LOGA-PCB               PIC X.                                   
       01  AVSP-WDK6-PCB               PIC X.                                   
       01  AVSP-WDA6B-PCB              PIC X.                                   
       01  AVSP-WDA6-PCB               PIC X.                                   
       01  AVSP-WDP4A-PCB              PIC X.                                   
       01  AVSP-WDB6-PCB               PIC X.                                   
       01  AVSP-PLATS-XXDM-PCB         PIC X.                                   
       01  AVSP-PLATS-XXDN-PCB         PIC X.                                   
       01  AVSP-PLATS-XXDP-PCB         PIC X.                                   
       01  AVSP-PLATS-XXDO-PCB         PIC X.                                   
       01  AVSP-PLATS-WDE6C-PCB        PIC X.                                   
       01  AVSP-PLATS-GMTC-PCB         PIC X.                                   
       01  AVSP-PLATS-WDB6-PCB         PIC X.                                   
       01  AVSP-DNOT-ORQP-PCB          PIC X.                                   
       01  AVSP-DNOT-ORQP2-PCB         PIC X.                                   
       01  AVSP-DNOT-ORQP3-PCB         PIC X.                                   
       01  AVSP-DNOT-4013-PCB          PIC X.                                   
       01  AVSP-DNOT-BENA-PCB          PIC X.                                   
       01  AVSP-PRQU-WDG2-PCB          PIC X.                                   
       01  AVSP-PRQU-WDC7-PCB          PIC X.                                   
       01  AVSP-PRQU-SJKO-WDK6-PCB     PIC X.                                   
       01  AVSP-PRNO-3107-PCB          PIC X.                                   
       01  AVSP-TMS-1165-PCB           PIC X.                                   
       01  AVSP-TMS-4141-PCB           PIC X.                                   
       01  AVSP-TMS-WDB2-PCB           PIC X.                                   
       01  AVSP-TMS-WDB6-PCB           PIC X.                                   
       01  AVSP-TMS-WDD3-PCB           PIC X.                                   
       01  AVSP-TMS-WDB1-PCB           PIC X.                                   
       01  AVSP-TMS-WDE4A-PCB          PIC X.                                   
       01  AVSP-TMS-WDE4F-PCB          PIC X.                                   
       01  AVSP-TMS-WDQ2-PCB           PIC X.                                   
       01  AVSP-TMS-WDQ3-PCB           PIC X.                                   
       01  AVSP-TMS-WDK6-PCB           PIC X.                                   
       01  AVSP-TMS-WDE6-PCB           PIC X.                                   
       01  AVSP-TMS-WDK5-PCB           PIC X.                                   
       01  AVSP-TMS-WDQ2C-PCB          PIC X.                                   
                                                                                
                                                                                
       PROCEDURE DIVISION  USING MSG-PCB DETTAPGM-PCB                           
           AVSP-4342-PCB    AVSP-2191-PCB  AVSP-4349-PCB                        
           TMS-CRE-PCB TMS-DEL-PCB                                              
           ATAB-PCB  EMBB-PCB WDE6-PCB ORQA-PCB WDE4-PCB                        
           AVSP-USEA-PCB                                                        
           AVSP-WDE41-PCB AVSP-WDE42-PCB   AVSP-WDE4-PCB AVSP-WDE4B-PCB         
           AVSP-WDE4E-PCB AVSP-WDE6-PCB    AVSP-XXDV-PCB AVSP-ORQA-PCB          
           AVSP-XXKW-PCB    AVSP-XXLB-PCB  AVSP-XXJK-PCB AVSP-ZZAC-PCB          
           AVSP-ORQI-PCB    AVSP-ORQL-PCB  AVSP-WDE6E-PCB AVSP-WDE62-PCB        
           AVSP-ORQICSQ-PCB                AVSP-ORQM-PCB                        
           AVSP-WDE4A-PCB AVSP-WDM2-PCB    AVSP-ORQA2-PCB                       
           AVSP-ORDP1-PCB AVSP-ORDP2-PCB   AVSP-XXJN-PCB AVSP-XXKH-PCB          
           AVSP-ARTC-PCB    AVSP-WDK7-PCB  AVSP-ARTM-PCB AVSP-AUTF-PCB          
           AVSP-4487-PCB    AVSP-4541-PCB  AVSP-LOGA-PCB                        
           AVSP-WDK6-PCB    AVSP-WDA6B-PCB AVSP-WDA6-PCB AVSP-WDP4A-PCB         
           AVSP-WDB6-PCB                                                        
           AVSP-PLATS-XXDM-PCB AVSP-PLATS-XXDN-PCB AVSP-PLATS-XXDP-PCB          
           AVSP-PLATS-XXDO-PCB AVSP-PLATS-WDE6C-PCB AVSP-PLATS-GMTC-PCB         
           AVSP-PLATS-WDB6-PCB                                                  
           AVSP-DNOT-ORQP-PCB                                                   
           AVSP-DNOT-ORQP2-PCB                                                  
           AVSP-DNOT-ORQP3-PCB                                                  
           AVSP-DNOT-4013-PCB                                                   
           AVSP-DNOT-BENA-PCB                                                   
           AVSP-PRQU-WDG2-PCB                                                   
           AVSP-PRQU-WDC7-PCB                                                   
           AVSP-PRQU-SJKO-WDK6-PCB                                              
           AVSP-PRNO-3107-PCB                                                   
           AVSP-TMS-1165-PCB                                                    
           AVSP-TMS-4141-PCB                                                    
           AVSP-TMS-WDB2-PCB                                                    
           AVSP-TMS-WDB6-PCB                                                    
           AVSP-TMS-WDD3-PCB                                                    
           AVSP-TMS-WDB1-PCB                                                    
           AVSP-TMS-WDE4A-PCB                                                   
           AVSP-TMS-WDE4F-PCB                                                   
           AVSP-TMS-WDQ2-PCB                                                    
           AVSP-TMS-WDQ3-PCB                                                    
           AVSP-TMS-WDK6-PCB                                                    
           AVSP-TMS-WDE6-PCB                                                    
           AVSP-TMS-WDK5-PCB                                                    
           AVSP-TMS-WDQ2C-PCB.                                                  
                                                                                
       MAIN SECTION.                                                            
           ENTRY 'DLITCBL' USING MSG-PCB DETTAPGM-PCB                           
           AVSP-4342-PCB    AVSP-2191-PCB  AVSP-4349-PCB                        
           TMS-CRE-PCB TMS-DEL-PCB                                              
           ATAB-PCB  EMBB-PCB WDE6-PCB ORQA-PCB WDE4-PCB                        
           AVSP-USEA-PCB                                                        
           AVSP-WDE41-PCB AVSP-WDE42-PCB   AVSP-WDE4-PCB AVSP-WDE4B-PCB         
           AVSP-WDE4E-PCB AVSP-WDE6-PCB    AVSP-XXDV-PCB AVSP-ORQA-PCB          
           AVSP-XXKW-PCB    AVSP-XXLB-PCB  AVSP-XXJK-PCB AVSP-ZZAC-PCB          
           AVSP-ORQI-PCB    AVSP-ORQL-PCB  AVSP-WDE6E-PCB AVSP-WDE62-PCB        
           AVSP-ORQICSQ-PCB                AVSP-ORQM-PCB                        
           AVSP-WDE4A-PCB AVSP-WDM2-PCB    AVSP-ORQA2-PCB                       
           AVSP-ORDP1-PCB   AVSP-ORDP2-PCB AVSP-XXJN-PCB AVSP-XXKH-PCB          
           AVSP-ARTC-PCB    AVSP-WDK7-PCB  AVSP-ARTM-PCB AVSP-AUTF-PCB          
           AVSP-4487-PCB    AVSP-4541-PCB AVSP-LOGA-PCB                         
           AVSP-WDK6-PCB    AVSP-WDA6B-PCB AVSP-WDA6-PCB AVSP-WDP4A-PCB         
           AVSP-WDB6-PCB                                                        
           AVSP-PLATS-XXDM-PCB AVSP-PLATS-XXDN-PCB AVSP-PLATS-XXDP-PCB          
           AVSP-PLATS-XXDO-PCB AVSP-PLATS-WDE6C-PCB AVSP-PLATS-GMTC-PCB         
           AVSP-PLATS-WDB6-PCB                                                  
           AVSP-DNOT-ORQP-PCB                                                   
           AVSP-DNOT-ORQP2-PCB                                                  
           AVSP-DNOT-ORQP3-PCB                                                  
           AVSP-DNOT-4013-PCB                                                   
           AVSP-DNOT-BENA-PCB                                                   
           AVSP-PRQU-WDG2-PCB                                                   
           AVSP-PRQU-WDC7-PCB                                                   
           AVSP-PRQU-SJKO-WDK6-PCB                                              
           AVSP-PRNO-3107-PCB                                                   
           AVSP-TMS-1165-PCB                                                    
           AVSP-TMS-4141-PCB                                                    
           AVSP-TMS-WDB2-PCB                                                    
           AVSP-TMS-WDB6-PCB                                                    
           AVSP-TMS-WDD3-PCB                                                    
           AVSP-TMS-WDB1-PCB                                                    
           AVSP-TMS-WDE4A-PCB                                                   
           AVSP-TMS-WDE4F-PCB                                                   
           AVSP-TMS-WDQ2-PCB                                                    
           AVSP-TMS-WDQ3-PCB                                                    
           AVSP-TMS-WDK6-PCB                                                    
           AVSP-TMS-WDE6-PCB                                                    
           AVSP-TMS-WDK5-PCB                                                    
           AVSP-TMS-WDQ2C-PCB.                                                  
                                                                                
           PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
           IF SUB-KDRC = 0                                                      
              PERFORM A-INIT                                                    
                                                                                
              IF REQU-WL0121I1 NOT = ALL '+'                                    
                 PERFORM B-KONTROLL-INDATA-RAD                                  
                 IF INDATA-OK                                                   
                    PERFORM C-KONTROLL-READ-DB                                  
                    IF INDATA-OK                                                
                       PERFORM D-UPDATE                                         
                       PERFORM E-READ-AGAIN-DB                                  
                       PERFORM F-CHECK-PRINT-OF-LISTS                           
                       PERFORM G-SET-KVRADER-NO-OF-LINES                        
                    END-IF                                                      
                 END-IF                                                         
                 IF RESP-IDMSG-ERROR = SPACE                                    
                    IF RESP-IDMSG-INFO = SPACE                                  
                       MOVE ERR-NO-DATA-ENTERED TO RESP-IDMSG-ERROR             
                    END-IF                                                      
                 END-IF                                                         
              ELSE                                                              
                 MOVE ERR-NO-DATA-ENTERED TO RESP-IDMSG-ERROR                   
              END-IF                                                            
                                                                                
              PERFORM S02-RETURN-RESPONSE                                       
           END-IF                                                               
                                                                                
           MOVE ZERO TO RETURN-CODE                                             
           GOBACK                                                               
           .                                                                    
           EJECT                                                                
       A-INIT SECTION.                                                          
           MOVE 'STA A-INI'  TO ERROR-TEXT                                      
                                                                                
           MOVE ALL '+' TO RESP-AREA                                            
           MOVE SPACE   TO RESP-IDMSG-ERROR                                     
                           RESP-IDMSG-INFO                                      
                           RESP-IDELMT-ERROR                                    
           MOVE '001'   TO RESP-IDMSGVER                                        
             MOVE ZERO                 TO      RESP-L128-KVRADER                
             MOVE ZERO                 TO      RESP-L129-KVRADER                
                                                                                
             MOVE REQU-FLSKRIV-CLABEL     TO RESP-FLSKRIV-CLABEL                
             MOVE REQU-FLSKRIV-DELNOTE    TO RESP-FLSKRIV-DELNOTE               
             MOVE +1                      TO RADIND                             
                                                                                
            ACCEPT WS-CURRENT-DATE FROM TIME                                    
            ACCEPT WS-CURRENT-TIME FROM DATE                                    
                                                                                
             PERFORM UNTIL RADIND > MAX-RADINDX                                 
                                                                                
               IF REQU-VKORDBTO-KOLLI (RADIND) NOT = ALL '+'                    
                 INSPECT REQU-VKORDBTO-KOLLI (RADIND) REPLACING                 
                 LEADING SPACE BY ZERO                                          
                 IF REQU-VKORDBTO-KOLLI (RADIND) NUMERIC                        
                   MOVE REQU-VKORDBTO-KOLLI (RADIND)                            
                   TO RESP-VKORDBTO-KOLLI (RADIND)                              
                 END-IF                                                         
               END-IF                                                           
      *        IF REQU-KDEMBTYP(RADIND) NOT = ALL '+'                           
      *          MOVE REQU-KDEMBTYP(RADIND) TO RESP-KDEMBTYP (RADIND)           
      *          INSPECT RESP-KDEMBTYP (RADIND) REPLACING                       
      *                  LEADING ZERO BY SPACE                                  
      *        END-IF                                                           
               IF REQU-FLAVSP  (RADIND) NOT = ALL '+'                           
                 MOVE REQU-FLAVSP (RADIND)  TO RESP-FLAVSP (RADIND)             
               END-IF                                                           
               ADD +1      TO RADIND                                            
             END-PERFORM                                                        
                                                                                
           PERFORM AB-NOLLA-ARB-TAB                                             
           .                                                                    
           EJECT                                                                
       AB-NOLLA-ARB-TAB         SECTION.                                        
           MOVE 'STA AB-NOLLA'    TO ERROR-TEXT                                 
                                                                                
           MOVE +1                TO RADIND                                     
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
             MOVE ZERO            TO WS-IDPRODNR       (RADIND)                 
                                     WS-IDKOLLI        (RADIND)                 
                                     WS-IDKUNDNR       (RADIND)                 
                                     WS-IDORDNR        (RADIND)                 
                                     WS-IDPLKLST       (RADIND)                 
                                     WS-KVORDRAD       (RADIND)                 
                                     WS-KDFRAKT        (RADIND)                 
                                     WS-IDKUNDRF       (RADIND)                 
                                     WS-VKTARA         (RADIND)                 
                                     WS-VKORDBTO       (RADIND)                 
                                     WS-KDEMBTYP       (RADIND)                 
                                     WS-DIKOLLIL       (RADIND)                 
                                     WS-DIKOLLIB       (RADIND)                 
                                     WS-DIKOLLIH       (RADIND)                 
                                     WS-KDKOLLID       (RADIND)                 
             MOVE SPACE           TO WS-KDKOLLI        (RADIND)                 
                                     WS-PRTVAL-FOLJES    (RADIND)               
                                     WS-PRTVAL-ADRESSFL (RADIND)                
             ADD +1               TO RADIND                                     
           END-PERFORM                                                          
           .                                                                    
           EJECT                                                                
       B-KONTROLL-INDATA-RAD    SECTION.                                        
           MOVE 'STA B-KOL'      TO ERROR-TEXT                                  
           MOVE JA TO KEYS-SW                                                   
                                                                                
      ***  KONTROLL AV REQU-KDPGMACT                                            
           IF REQU-KDPGMACT = 'S' OR 'E'                                        
              CONTINUE                                                          
           ELSE                                                                 
              MOVE SYSTEM-ERROR       TO RESP-IDMSG-ERROR                       
              MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
              MOVE NEJ TO KEYS-SW                                               
           END-IF                                                               
                                                                                
           MOVE REQU-KDMATT           TO WS-KDMATT                              
                                                                                
           MOVE REQU-FLSKRIV-CLABEL   TO RESP-FLSKRIV-CLABEL                    
           MOVE REQU-FLSKRIV-DELNOTE  TO RESP-FLSKRIV-DELNOTE                   
           PERFORM BJ-CHECK-REQU-IDKOLLI-SAMP                                   
                                                                                
           MOVE +1         TO RADIND                                            
                                                                                
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
                                                                                
             IF  REQU-RAD       (RADIND) = ALL '+'                              
             OR  REQU-FLAVSP    (RADIND) = RAETT                                
      *        TOM RAD ELLER REDAN BEHANDLAD                                    
               CONTINUE                                                         
             ELSE                                                               
      *        IFYLLD RAD - OBEHANDLAD ELLER TIDIGARE FEL                       
                                                                                
               PERFORM BC-CHECK-REQU-IDPRODNR-PLKLST                            
                                                                                
               PERFORM BD-CHECK-REQU-IDKOLLI                                    
                                                                                
               PERFORM BE-CHECK-REQU-KDKOLLI-KDEMBTYP                           
                                                                                
               PERFORM BF-CHECK-REQU-VKORDBTO                                   
                                                                                
               PERFORM BG-CHECK-REQU-KOLLI-DIMENSION                            
                                                                                
             END-IF                                                             
                                                                                
             ADD +1        TO RADIND                                            
           END-PERFORM                                                          
                                                                                
           IF INDATA-OK                                                         
              PERFORM BI-CHECK-PRODNR-PLKLST                                    
      **** KONTROLL AV ATT INGA DUBLETTER (IDPRODNR) RAPPORTERAS                
           END-IF                                                               
                                                                                
           .                                                                    
           EJECT                                                                
       BC-CHECK-REQU-IDPRODNR-PLKLST      SECTION.                              
           MOVE 'STA BC-CHECK'      TO ERROR-TEXT                               
                                                                                
           IF REQU-IDPRODNR (RADIND) = ALL '+'                                  
              MOVE ZERO              TO WS-IDPRODNR (RADIND)                    
                                                                                
              MOVE MUST-BE-ENTERED   TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
              MOVE 'IDPRODNR'        TO RESP-IDELMT-ERROR                       
              MOVE NEJ               TO INDATA-SW                               
           ELSE                                                                 
             INSPECT REQU-IDPRODNR(RADIND) REPLACING                            
                     LEADING SPACE BY ZERO                                      
             IF REQU-IDPRODNR (RADIND) NUMERIC                                  
                MOVE REQU-IDPRODNR(RADIND) TO WS-IDPRODNR (RADIND)              
                MOVE REQU-IDPRODNR(RADIND) TO RESP-IDPRODNR (RADIND)            
             ELSE                                                               
                MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
                MOVE 'IDPRODNR'      TO RESP-IDELMT-ERROR                       
                MOVE NEJ             TO INDATA-SW                               
                MOVE RADIND          TO RADIND-NUM9                             
             END-IF                                                             
           END-IF                                                               
                                                                                
           IF REQU-IDPLKLST (RADIND) = ALL '+'                                  
              MOVE ZERO              TO WS-IDPLKLST (RADIND)                    
                                        REQU-IDPLKLST (RADIND)                  
                                                                                
              MOVE MUST-BE-ENTERED   TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
              MOVE 'IDPLKLST'        TO RESP-IDELMT-ERROR                       
              MOVE NEJ               TO INDATA-SW                               
           ELSE                                                                 
             INSPECT REQU-IDPLKLST(RADIND) REPLACING                            
                     LEADING SPACE BY ZERO                                      
             IF REQU-IDPLKLST (RADIND) NUMERIC                                  
                MOVE REQU-IDPLKLST(RADIND) TO WS-IDPLKLST (RADIND)              
                MOVE REQU-IDPLKLST(RADIND) TO RESP-IDPLKLST (RADIND)            
             ELSE                                                               
                MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
                MOVE 'IDPLKLST'      TO RESP-IDELMT-ERROR                       
                MOVE NEJ                   TO INDATA-SW                         
             END-IF                                                             
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       BD-CHECK-REQU-IDKOLLI                SECTION.                            
           MOVE 'STA BD-CHECK'      TO ERROR-TEXT                               
                                                                                
           IF REQU-IDKOLLI (RADIND) = ALL '+'                                   
              MOVE ZERO              TO WS-IDKOLLI (RADIND)                     
                                        REQU-IDKOLLI (RADIND)                   
                                                                                
              MOVE MUST-BE-ENTERED   TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
              MOVE 'IDKOLLI'         TO RESP-IDELMT-ERROR                       
              MOVE NEJ               TO INDATA-SW                               
           ELSE                                                                 
             INSPECT REQU-IDKOLLI(RADIND) REPLACING                             
                     LEADING SPACE BY ZERO                                      
             IF  REQU-IDKOLLI (RADIND) NUMERIC                                  
               IF REQU-IDKOLLI (RADIND) = ZERO                                  
                                                                                
                  MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                      
                                          RESP-IDMSG-ERROR-RAD (RADIND)         
                  MOVE 'IDKOLLI'       TO RESP-IDELMT-ERROR                     
                  MOVE NEJ             TO INDATA-SW                             
               ELSE                                                             
                  MOVE REQU-IDKOLLI (RADIND) TO WS-IDKOLLI (RADIND)             
                  MOVE REQU-IDKOLLI(RADIND) TO RESP-IDKOLLI (RADIND)            
               END-IF                                                           
             ELSE                                                               
               MOVE MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
               MOVE 'IDKOLLI'        TO RESP-IDELMT-ERROR                       
               MOVE NEJ              TO INDATA-SW                               
             END-IF                                                             
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       BE-CHECK-REQU-KDKOLLI-KDEMBTYP       SECTION.                            
           MOVE 'STA BE-CHECK'      TO ERROR-TEXT                               
                                                                                
           IF REQU-KDKOLLI (RADIND) = ALL '+'                                   
             CONTINUE                                                           
           ELSE                                                                 
              MOVE REQU-KDKOLLI (RADIND) TO WS-KDKOLLI (RADIND)                 
              MOVE REQU-KDKOLLI (RADIND) TO RESP-KDKOLLI (RADIND)               
           END-IF                                                               
                                                                                
           IF REQU-KDEMBTYP (RADIND) = ALL '+'                                  
             CONTINUE                                                           
           ELSE                                                                 
             INSPECT REQU-KDEMBTYP (RADIND) REPLACING                           
                     LEADING SPACE BY ZERO                                      
             IF (REQU-KDEMBTYP (RADIND) NUMERIC)                                
             AND (REQU-KDEMBTYP (RADIND) > 0 AND < 8)                           
             AND (REQU-KDKOLLI (RADIND) = ALL '+')                              
                MOVE REQU-KDEMBTYP (RADIND) TO WS-KDEMBTYP (RADIND)             
                MOVE REQU-KDEMBTYP (RADIND) TO RESP-KDEMBTYP (RADIND)           
             ELSE                                                               
               IF REQU-KDKOLLI (RADIND) = ALL '+'                               
                  MOVE MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR                     
                                           RESP-IDMSG-ERROR-RAD (RADIND)        
                  MOVE 'KDEMBTYP'       TO RESP-IDELMT-ERROR                    
               ELSE                                                             
                  MOVE IS-INVALID       TO RESP-IDMSG-ERROR                     
                                           RESP-IDMSG-ERROR-RAD (RADIND)        
                  MOVE 'KDEMBTYP'       TO RESP-IDELMT-ERROR                    
               END-IF                                                           
               MOVE NEJ              TO INDATA-SW                               
             END-IF                                                             
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       BF-CHECK-REQU-VKORDBTO               SECTION.                            
           MOVE 'STA BF-CHECK'      TO ERROR-TEXT                               
                                                                                
           IF REQU-VKORDBTO-KOLLI (RADIND) = ALL '+'                            
             MOVE ZERO              TO  WS-VKORDBTO (RADIND)                    
           ELSE                                                                 
                                                                                
              INSPECT REQU-VKORDBTO-KOLLI (RADIND) REPLACING                    
                     LEADING SPACE BY ZERO                                      
              MOVE REQU-VKORDBTO-KOLLI (RADIND) TO DEC-IDFRIDATA                
              MOVE 5                     TO DEC-KVHELTAL                        
              MOVE 1                     TO DEC-KVDECIMAL                       
              CALL WDECEDIT USING DEC-WDECAREA                                  
                                                                                
              IF DEC-KDSVAR-OK AND DEC-IDEDITDATA > ZERO                        
                 MOVE DEC-IDEDITDATA  TO  WS-VKORDBTO (RADIND)                  
               MOVE WS-VKORDBTO (RADIND) TO RESP-VKORDBTO-KOLLI (RADIND)        
              ELSE                                                              
                 MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                       
                                         RESP-IDMSG-ERROR-RAD (RADIND)          
                 MOVE 'VKORDBTO'      TO RESP-IDELMT-ERROR                      
                 MOVE NEJ             TO INDATA-SW                              
              END-IF                                                            
              INSPECT WS-VKORDBTO (RADIND) REPLACING                            
                     LEADING SPACE BY ZERO                                      
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       BG-CHECK-REQU-KOLLI-DIMENSION        SECTION.                            
           MOVE 'STA BG-CHECK'      TO ERROR-TEXT                               
                                                                                
           IF REQU-KDKOLLI (RADIND)  =  ALL '+' AND                             
             (REQU-DIKOLLIL (RADIND) = (ALL '+' OR ZERO)  OR                    
              REQU-DIKOLLIB (RADIND) = (ALL '+' OR ZERO)  OR                    
              REQU-DIKOLLIH (RADIND) = (ALL '+' OR ZERO))                       
              MOVE NEJ                       TO INDATA-SW                       
              MOVE ERR-MORE-CASE-INFO-NEEDED TO RESP-IDMSG-ERROR                
           ELSE                                                                 
              IF REQU-DIKOLLIL (RADIND) = ALL '+'                               
                 CONTINUE                                                       
              ELSE                                                              
                INSPECT REQU-DIKOLLIL (RADIND) REPLACING                        
                        LEADING SPACE BY ZERO                                   
                IF REQU-DIKOLLIL (RADIND) NUMERIC                               
                AND REQU-DIKOLLIL (RADIND) > ZERO                               
                  MOVE REQU-DIKOLLIL (RADIND) TO WS-DIKOLLIL (RADIND)           
                  MOVE REQU-DIKOLLIL (RADIND) TO RESP-DIKOLLIL (RADIND)         
                ELSE                                                            
                   MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                     
                                           RESP-IDMSG-ERROR-RAD (RADIND)        
                   MOVE 'DIKOLLIL'      TO RESP-IDELMT-ERROR                    
                   MOVE NEJ             TO INDATA-SW                            
                END-IF                                                          
              END-IF                                                            
                                                                                
              IF REQU-DIKOLLIB (RADIND) = ALL '+'                               
                 CONTINUE                                                       
              ELSE                                                              
                INSPECT REQU-DIKOLLIB (RADIND) REPLACING                        
                         LEADING SPACE BY ZERO                                  
                IF REQU-DIKOLLIB (RADIND) NUMERIC                               
                AND REQU-DIKOLLIB (RADIND) > ZERO                               
                  MOVE REQU-DIKOLLIB (RADIND) TO WS-DIKOLLIB (RADIND)           
                  MOVE REQU-DIKOLLIB (RADIND) TO RESP-DIKOLLIB (RADIND)         
                ELSE                                                            
                  MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                      
                                          RESP-IDMSG-ERROR-RAD (RADIND)         
                  MOVE 'DIKOLLIB'      TO RESP-IDELMT-ERROR                     
                  MOVE NEJ             TO INDATA-SW                             
                END-IF                                                          
              END-IF                                                            
                                                                                
              IF REQU-DIKOLLIH (RADIND) = ALL '+'                               
                 CONTINUE                                                       
              ELSE                                                              
                INSPECT REQU-DIKOLLIH (RADIND) REPLACING                        
                        LEADING SPACE BY ZERO                                   
                IF REQU-DIKOLLIH (RADIND) NUMERIC                               
                AND REQU-DIKOLLIH (RADIND) > ZERO                               
                  MOVE REQU-DIKOLLIH (RADIND) TO WS-DIKOLLIH (RADIND)           
                  MOVE REQU-DIKOLLIH (RADIND) TO RESP-DIKOLLIH (RADIND)         
                ELSE                                                            
                  MOVE MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                      
                                          RESP-IDMSG-ERROR-RAD (RADIND)         
                  MOVE 'DIKOLLIH'      TO RESP-IDELMT-ERROR                     
                  MOVE NEJ             TO INDATA-SW                             
                END-IF                                                          
              END-IF                                                            
           END-IF                                                               
                                                                                
           IF INDATA-OK AND US-MEASUREMENT                                      
              COMPUTE WS-DIKOLLIL (RADIND) =                                    
                      CONV-IN-TO-CM * WS-DIKOLLIL (RADIND)                      
              END-COMPUTE                                                       
              COMPUTE WS-DIKOLLIB (RADIND) =                                    
                      CONV-IN-TO-CM * WS-DIKOLLIB (RADIND)                      
              END-COMPUTE                                                       
              COMPUTE WS-DIKOLLIH (RADIND) =                                    
                      CONV-IN-TO-CM * WS-DIKOLLIH (RADIND)                      
              END-COMPUTE                                                       
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       BI-CHECK-PRODNR-PLKLST                    SECTION.                       
           MOVE 'STA BI-CHECK'      TO ERROR-TEXT                               
           MOVE +1         TO RADIND                                            
                                                                                
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
              IF  REQU-IDPRODNR (RADIND) NOT = ALL '+'                          
              AND REQU-FLAVSP  (RADIND)     = '+'                               
                 MOVE RADIND   TO JMF-IND                                       
                 ADD +1        TO JMF-IND                                       
                                                                                
                 PERFORM UNTIL JMF-IND > MAX-RADINDX                            
                    IF REQU-IDPRODNR (JMF-IND) NOT = ALL '+'                    
                                                                                
                       IF (REQU-IDPRODNR (RADIND) =                             
                           REQU-IDPRODNR (JMF-IND)                              
                       AND                                                      
                           REQU-IDPLKLST (RADIND) =                             
                           REQU-IDPLKLST (JMF-IND))                             
                                                                                
                          MOVE DUPLICATE-LINES TO RESP-IDMSG-ERROR              
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
                          MOVE 'IDPRODNR' TO RESP-IDELMT-ERROR                  
                          MOVE NEJ TO INDATA-SW                                 
                          MOVE 'DUP' TO RESP-IDMSG-ERROR-RAD (JMF-IND)          
                       END-IF                                                   
                       IF (REQU-IDPRODNR (RADIND) =                             
                           REQU-IDPRODNR (JMF-IND)                              
                       AND                                                      
                           REQU-IDKOLLI (RADIND) =                              
                           REQU-IDKOLLI (JMF-IND))                              
                          MOVE DUPLICATE-LINES TO RESP-IDMSG-ERROR              
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
                          MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                   
                          MOVE NEJ TO INDATA-SW                                 
                          MOVE 'DUP' TO RESP-IDMSG-ERROR-RAD (JMF-IND)          
                                                                                
                       END-IF                                                   
                    END-IF                                                      
                    ADD +1        TO JMF-IND                                    
                 END-PERFORM                                                    
                                                                                
              END-IF                                                            
              ADD +1        TO RADIND                                           
           END-PERFORM                                                          
           .                                                                    
           SKIP2                                                                
       BJ-CHECK-REQU-IDKOLLI-SAMP    SECTION.                                   
           MOVE 'STA BJ-CHECK'      TO ERROR-TEXT                               
                                                                                
           MOVE ZERO                   TO RESP-IDKOLLI-SAMP                     
           .                                                                    
           SKIP2                                                                
       C-KONTROLL-READ-DB         SECTION.                                      
           MOVE 'STA C-KONTROLL'   TO ERROR-TEXT                                
                                                                                
           MOVE +1               TO RADIND                                      
                                                                                
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
                                                                                
             IF  REQU-RAD      (RADIND) NOT = ALL '+'                           
             AND REQU-FLAVSP  (RADIND)  = '+'                                   
                                                                                
               MOVE REQU-IDPRODNR (RADIND) TO W-IDPRODNR                        
                                              W-IDPRODNR-WDQ3D                  
                                                                                
               PERFORM CA-CHECK-KDKOLLI-KDEMBTYP                                
                                                                                
               PERFORM CB-CHECK-REQU-VS-WDE6                                    
                                                                                
               IF INDATA-OK                                                     
                 PERFORM CC-CHECK-ORDERPART-ON-WDQ3                             
               END-IF                                                           
                                                                                
               IF INDATA-OK                                                     
                 PERFORM CD-CHECK-KDKOLLI-DIKOLLI                               
               END-IF                                                           
                                                                                
               IF INDATA-OK                                                     
                 PERFORM CE-CHECK-WDE4-AND-VKORDBTO                             
               END-IF                                                           
               IF INDATA-FEL                                                    
                 MOVE RADIND       TO RADIND-NUM9                               
                 MOVE +99          TO RADIND                                    
               END-IF                                                           
                                                                                
             END-IF                                                             
             ADD +1                TO RADIND                                    
           END-PERFORM                                                          
                                                                                
           IF INDATA-FEL                                                        
             ADD +1                TO RADIND-NUM9                               
             MOVE RADIND-NUM9      TO RADIND                                    
           END-IF                                                               
           .                                                                    
                                                                                
       CA-CHECK-KDKOLLI-KDEMBTYP         SECTION.                               
           MOVE 'STA CA-CHECK'        TO ERROR-TEXT                             
                                                                                
           IF REQU-KDKOLLI (RADIND) NOT = ALL '+'                               
              IF REQU-KDEMBTYP (RADIND) NOT = ALL '+'                           
                 MOVE IS-INVALID      TO RESP-IDMSG-ERROR                       
                                         RESP-IDMSG-ERROR-RAD (RADIND)          
                 MOVE 'KDEMBTYP'   TO RESP-IDELMT-ERROR                         
                 MOVE NEJ          TO INDATA-SW                                 
              END-IF                                                            
           ELSE                                                                 
              IF REQU-KDEMBTYP (RADIND) = ALL '+'                               
                 MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                       
                                         RESP-IDMSG-ERROR-RAD (RADIND)          
                 MOVE 'KDKOLLI'    TO RESP-IDELMT-ERROR                         
                 MOVE NEJ          TO INDATA-SW                                 
              END-IF                                                            
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       CB-CHECK-REQU-VS-WDE6             SECTION.                               
           MOVE 'STA CB-CHECK'        TO ERROR-TEXT                             
                                                                                
           MOVE WS-IDPRODNR (RADIND)  TO W-IDPRODNR                             
           MOVE WS-IDKOLLI  (RADIND)  TO W-IDKOLLI                              
                                                                                
           PERFORM IMS-GU-WDE601                                                
           IF SEGMENT-FINNS                                                     
                                                                                
             IF VORD-IDDC = REQU-IDDC-KEY                                       
                                                                                
               IF VORD-KDMETOD = +3                                             
                 MOVE 'IDPRODNR'          TO RESP-IDELMT-ERROR                  
                 MOVE NEJ                 TO INDATA-SW                          
      *          MOVE ERR-USESCREEN-L0197-ORL0199 TO RESP-IDMSG-ERROR           
                 MOVE ERR-ORDER-HAS-WRONG-STATUS  TO RESP-IDMSG-ERROR           
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
               ELSE                                                             
                                                                                
                 IF VORD-FLMANORD = NEJ                                         
                   MOVE VORD-IDDISTR      TO WS-IDDISTR  (RADIND)               
                                             TEST-IDDISTR                       
                   MOVE VORD-IDKUNDNR     TO WS-IDKUNDNR (RADIND)               
                                                                                
                   IF REQU-IDKOLLI (RADIND) NOT = ALL '+'                       
                     PERFORM IMS-GNP-WDE611                                     
                     IF SEGMENT-FINNS                                           
                      MOVE 'IDKOLLI' TO RESP-IDELMT-ERROR                       
                      MOVE NEJ         TO INDATA-SW                             
                      MOVE ERR-CASE-ALREADY-REPORT TO RESP-IDMSG-ERROR          
                                     RESP-IDMSG-ERROR-RAD (RADIND)              
                     END-IF                                                     
                   END-IF                                                       
                 ELSE                                                           
                   IF INDATA-OK                                                 
                     MOVE NEJ        TO INDATA-SW                               
                     MOVE 'IDPRODNR' TO RESP-IDELMT-ERROR                       
                     MOVE ERR-WRONG-LINES TO RESP-IDMSG-ERROR                   
                                         RESP-IDMSG-ERROR-RAD (RADIND)          
                   END-IF                                                       
                 END-IF                                                         
               END-IF                                                           
                                                                                
             ELSE                                                               
               IF INDATA-OK                                                     
                 MOVE NEJ            TO INDATA-SW                               
                 MOVE WRONG-DC       TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
                 MOVE 'IDDC'         TO RESP-IDELMT-ERROR                       
               END-IF                                                           
             END-IF                                                             
                                                                                
           ELSE                                                                 
              MOVE NEJ               TO INDATA-SW                               
              MOVE 'IDPRODNR'        TO RESP-IDELMT-ERROR                       
              MOVE NOT-FOUND         TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       CC-CHECK-ORDERPART-ON-WDQ3    SECTION.                                   
           MOVE 'STA CC-CHECK'        TO ERROR-TEXT                             
                                                                                
            MOVE WS-IDPRODNR(RADIND)  TO W-IDPRODNR-WDQ3D                       
            MOVE WS-IDPLKLST(RADIND)  TO W-IDPLKLST-WDQ3D                       
            PERFORM IMS-GU-WDQ3D1                                               
                                                                                
            IF SEGMENT-SAKNAS                                                   
               MOVE 'IDPRODNR'       TO RESP-IDELMT-ERROR                       
               MOVE NOT-FOUND        TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
               MOVE NEJ              TO INDATA-SW                               
            ELSE                                                                
               MOVE ODEL-IDORDNR7        TO WS-IDORDNR (RADIND)                 
               MOVE ODEL-IDUSER          TO WS-IDUSER  (RADIND)                 
                                                                                
      ** SOFTWARE KONTROLL                                                      
               IF (ODEL-IDLEVNR = '1441 ' OR                                    
                  ODEL-IDLEVNR = 'BP2TW')  AND                                  
                  ODEL-IDPRC = '9998'                                           
                  MOVE NEJ          TO INDATA-SW                                
                  MOVE NO-REPORTING-FOR-DIRECT-SUPPLI                           
                    TO RESP-IDMSG-ERROR                                         
                       RESP-IDMSG-ERROR-RAD (RADIND)                            
               ELSE                                                             
                 IF  ODEL-KDODELSTA    = 'U'                                    
                 AND ODEL-KVPACKRAD-OD = ZERO                                   
                    CONTINUE                                                    
                 ELSE                                                           
                   MOVE NEJ              TO INDATA-SW                           
                   MOVE ERR-WRONG-STATUS TO RESP-IDMSG-ERROR                    
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
                   IF ODEL-KDODELSTA = 'P'    AND                               
                      ODEL-KVPACKRAD-OD > ZERO                                  
                     MOVE ERR-ORDER-PART-READY TO RESP-IDMSG-ERROR              
                   END-IF                                                       
                 END-IF                                                         
               END-IF                                                           
            END-IF                                                              
           .                                                                    
           SKIP2                                                                
       CD-CHECK-KDKOLLI-DIKOLLI          SECTION.                               
           MOVE 'STA CD-CHECK'        TO ERROR-TEXT                             
           MOVE ZERO                  TO WS-KOLLI-VLORDBTO                      
                                                                                
           IF REQU-KDKOLLI (RADIND) NOT = ALL '+'                               
              MOVE REQU-KDKOLLI (RADIND) TO W-KDKOLLI-WDK5                      
              PERFORM IMS-GU-EMBB                                               
              IF SEGMENT-SAKNAS                                                 
                 MOVE 'KDKOLLI'    TO RESP-IDELMT-ERROR                         
                 MOVE '025'        TO RESP-IDMSG-ERROR                          
                                      RESP-IDMSG-ERROR-RAD (RADIND)             
                 MOVE NEJ          TO INDATA-SW                                 
              ELSE                                                              
                 MOVE EMB-KDEMBTYP TO WS-KDEMBTYP (RADIND)                      
                 MOVE EMB-VKTARA   TO WS-VKTARA   (RADIND)                      
                 MOVE EMB-KDKOLLID TO WS-KDKOLLID (RADIND)                      
                                                                                
                 IF REQU-DIKOLLIL (RADIND) = (ALL '+' OR ZERO)                  
                    IF EMB-DIKOLLIL = +0                                        
                       MOVE 'DIKOLLIL'      TO RESP-IDELMT-ERROR                
                       MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
                       MOVE NEJ             TO INDATA-SW                        
                    ELSE                                                        
                       MOVE EMB-DIKOLLIL    TO WS-DIKOLLIL (RADIND)             
                    END-IF                                                      
                 END-IF                                                         
                                                                                
                 IF REQU-DIKOLLIB (RADIND) = (ALL '+' OR ZERO)                  
                    IF EMB-DIKOLLIB = +0                                        
                       MOVE 'DIKOLLIB'      TO RESP-IDELMT-ERROR                
                       MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
                       MOVE NEJ             TO INDATA-SW                        
                    ELSE                                                        
                       MOVE EMB-DIKOLLIB    TO WS-DIKOLLIB (RADIND)             
                    END-IF                                                      
                 END-IF                                                         
                                                                                
                 IF REQU-DIKOLLIH (RADIND) = (ALL '+' OR ZERO)                  
                    IF EMB-DIKOLLIH = +0                                        
                       MOVE 'DIKOLLIH'      TO RESP-IDELMT-ERROR                
                       MOVE MUST-BE-ENTERED TO RESP-IDMSG-ERROR                 
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
                       MOVE NEJ             TO INDATA-SW                        
                    ELSE                                                        
                       MOVE EMB-DIKOLLIH    TO WS-DIKOLLIH (RADIND)             
                    END-IF                                                      
                 END-IF                                                         
              END-IF                                                            
           END-IF                                                               
                                                                                
           COMPUTE WS-KOLLI-VLORDBTO = WS-DIKOLLIL (RADIND) *                   
                     WS-DIKOLLIB (RADIND) * WS-DIKOLLIH (RADIND)                
           END-COMPUTE                                                          
                                                                                
           .                                                                    
           SKIP2                                                                
       CE-CHECK-WDE4-AND-VKORDBTO        SECTION.                               
           MOVE 'STA CE-CHECK'        TO ERROR-TEXT                             
                                                                                
           MOVE WS-IDDISTR  (RADIND)     TO W-401-IDDISTR                       
           MOVE WS-IDKUNDNR (RADIND)     TO W-401-IDKUNDNR                      
           MOVE WS-IDORDNR  (RADIND)     TO W-401-IDORDNR                       
           MOVE WS-IDPRODNR (RADIND)     TO W-401-IDPRODNR                      
           MOVE WS-IDPLKLST (RADIND)     TO W-401-IDPLKLST                      
           MOVE ZERO                     TO WS-KORD-VKORDNTO (RADIND)           
           MOVE ZERO                     TO WS-ORAD-VLORDNTO-SUM                
                                                                                
           PERFORM IMS-GU-WDE401                                                
           IF SEGMENT-FINNS                                                     
             IF KORD-KVORDRAD-PACK = ZERO                                       
               MOVE KORD-VKORDNTO     TO WS-KORD-VKORDNTO     (RADIND)          
               MOVE KORD-IDORDER      TO WS-IDORDER           (RADIND)          
                                                                                
               PERFORM IMS-GNP-WDE4                                             
               IF SEGMENT-FINNS                                                 
                                                                                
                  PERFORM UNTIL SEGMENT-SAKNAS OR INDATA-FEL                    
                                                                                
                     PERFORM CEA-KONTROLLERA-RADER                              
                     PERFORM IMS-GNP-WDE411                                     
                  END-PERFORM                                                   
                                                                                
                                                                                
                  PERFORM CEB-COMP-VIKT-BRUTTO                                  
               ELSE                                                             
                  MOVE 'IDPRODNR'    TO RESP-IDELMT-ERROR                       
                  MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                  
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
                  MOVE NEJ           TO INDATA-SW                               
               END-IF                                                           
             ELSE                                                               
                MOVE NEJ             TO INDATA-SW                               
                MOVE 'IDPRODNR'      TO RESP-IDELMT-ERROR                       
                MOVE ERR-ORDER-PART-READY      TO RESP-IDMSG-ERROR              
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
             END-IF                                                             
           ELSE                                                                 
               MOVE NEJ              TO INDATA-SW                               
               MOVE 'IDPLKLST'       TO RESP-IDELMT-ERROR                       
               MOVE ERR-ORDER-PARTS-MISSING TO RESP-IDMSG-INFO                  
                                        RESP-IDMSG-ERROR-RAD (RADIND)           
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       CEA-KONTROLLERA-RADER         SECTION.                                   
                                                                                
           MOVE 'STA CEA-KONTROLL'     TO ERROR-TEXT                            
                                                                                
           IF ORAD-IDLEVNR NOT = SPACE                                          
             MOVE JA                  TO DIRLEV-KOLLI-SW                        
           END-IF                                                               
      *                                                                         
           IF ORAD-KDRADSTA > 3                                                 
              MOVE NEJ               TO INDATA-SW                               
              MOVE RADIND TO RAD-DISPLAY                                        
              MOVE 'IDPRODNR'       TO RESP-IDELMT-ERROR                        
              MOVE NEJ              TO INDATA-SW                                
              MOVE ERR-ORDER-REGISTRED      TO RESP-IDMSG-ERROR                 
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
           ELSE                                                                 
             IF ORAD-KVLEVART > ZERO                                            
                MOVE NEJ            TO INDATA-SW                                
                MOVE 'IDPRODNR'     TO RESP-IDELMT-ERROR                        
                MOVE ERR-CASE-REPORT-STARTED TO RESP-IDMSG-ERROR                
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
             ELSE                                                               
               IF ORAD-FLNOLLJ = JA                                             
                  MOVE NEJ          TO INDATA-SW                                
                  MOVE 'IDPRODNR'   TO RESP-IDELMT-ERROR                        
                  MOVE ERR-LINE-ALREADY-ZEROED  TO RESP-IDMSG-ERROR             
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
                                                                                
      *LK** VOLUME CONTROL *                                                    
           IF NOT DIST44-SCRAP-DIST                                             
             MOVE REQU-IDDC-KEY        TO VOL-IDDC                              
             IF CONTROL-OF-CASE-NET-VOLUME                                      
               COMPUTE WS-ORAD-VLORDNTO =                                       
                       ORAD-KVAVBART * ORAD-VLARTNTO                            
               END-COMPUTE                                                      
                                                                                
               ADD WS-ORAD-VLORDNTO    TO WS-ORAD-VLORDNTO-SUM                  
               IF WS-ORAD-VLORDNTO-SUM > WS-KOLLI-VLORDBTO                      
                                                                                
                 MOVE NEJ        TO INDATA-SW                                   
                 MOVE 'VLART'    TO RESP-IDELMT-ERROR                           
                 MOVE '117'      TO RESP-IDMSG-ERROR                            
                 MOVE 'VOL'      TO RESP-IDMSG-ERROR-RAD (RADIND)               
               END-IF                                                           
                                                                                
               MOVE ZERO         TO WS-ORAD-VLORDNTO                            
             END-IF                                                             
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       CEB-COMP-VIKT-BRUTTO          SECTION.                                   
           MOVE 'STA CEB-COMP-'         TO ERROR-TEXT                           
                                                                                
           IF REQU-VKORDBTO-KOLLI (RADIND) = ALL '+'                            
                                                                                
               COMPUTE WS-VKORDBTO (RADIND) =                                   
                    WS-KORD-VKORDNTO (RADIND) + WS-VKTARA (RADIND)              
               END-COMPUTE                                                      
                                                                                
               IF WS-VKORDBTO (RADIND) > ZERO                                   
                 MOVE WS-VKORDBTO(RADIND) TO WS-RESP-VKORDBTO                   
                 MOVE WS-RESP-VKORDBTO     TO                                   
                      RESP-VKORDBTO-KOLLI (RADIND)                              
               ELSE                                                             
                 MOVE '1'                TO WS-VKORDBTO-HK                      
                 MOVE WS-VKORDBTO-RED    TO WS-VKORDBTO        (RADIND)         
                                            WS-RESP-VKORDBTO                    
                 MOVE WS-RESP-VKORDBTO     TO                                   
                      RESP-VKORDBTO-KOLLI (RADIND)                              
               END-IF                                                           
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
           EJECT                                                                
       D-UPDATE  SECTION.                                                       
           MOVE 'STA D-UPDATE '         TO ERROR-TEXT                           
                                                                                
           MOVE +1          TO RADIND                                           
                                                                                
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
                                                                                
             IF  REQU-RAD      (RADIND) NOT = ALL '+'                           
             AND REQU-FLAVSP   (RADIND)    = '+'                                
                 PERFORM DA-INIT-AVSP-CALL                                      
                 PERFORM DB-AVSP-CALL                                           
                 IF AVSP-KDSVAR = SPACE                                         
                    PERFORM S15-RENSA-RAD                                       
                    PERFORM DF-LADDA-L198                                       
                    MOVE JA              TO ORDERDEL-PACKAD-SW                  
                    MOVE RAETT           TO REQU-FLAVSP (RADIND)                
                    PERFORM IMS-PURG-TRANS4349                                  
                 ELSE                                                           
                    PERFORM DC-ERROR-MESSAGE-TO-MOD                             
                    MOVE NEJ             TO ORDERDEL-PACKAD-SW                  
                    MOVE FEL-FR-W403AVSP TO REQU-FLAVSP (RADIND)                
                 END-IF                                                         
             END-IF                                                             
             ADD +1      TO RADIND                                              
           END-PERFORM                                                          
                                                                                
                                                                                
           PERFORM DE-CHECK-IF-INDATA-EXISTS                                    
                                                                                
           IF ORDERDEL-PACKAD AND INDATA-SAKNAS                                 
              IF INDATA-OK                                                      
                                                                                
                MOVE +1     TO RADIND                                           
                PERFORM UNTIL RADIND > MAX-RADINDX                              
                  MOVE ERR-UPDATED TO RESP-IDMSG-INFO                           
                  MOVE ZERO              TO RESP-IDPRODNR (RADIND)              
                                             RESP-IDPLKLST (RADIND)             
                                             RESP-IDKOLLI (RADIND)              
                                             RESP-KDEMBTYP (RADIND)             
                                             RESP-DIKOLLIL (RADIND)             
                                             RESP-DIKOLLIB (RADIND)             
                                             RESP-DIKOLLIH (RADIND)             
                                             RESP-VKORDBTO-KOLLI(RADIND)        
                  MOVE SPACE             TO RESP-KDKOLLI (RADIND)               
                                            RESP-FLAVSP  (RADIND)               
                                    RESP-IDMSG-ERROR-RAD  (RADIND)              
                  ADD +1 TO RADIND                                              
                END-PERFORM                                                     
              END-IF                                                            
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       DA-INIT-AVSP-CALL           SECTION.                                     
           MOVE 'STA DA-INIT-'         TO ERROR-TEXT                            
                                                                                
           MOVE REQU-IDDC-KEY                TO   AVSP-IDDC                     
           MOVE WS-IDANSTNR      (RADIND)    TO   AVSP-IDANSTNR                 
                                                                                
           MOVE 0121                         TO   AVSP-IDTRANS                  
           MOVE ZERO                         TO   AVSP-IDKOLLI-SAMP             
           MOVE WS-IDDISTR       (RADIND)    TO   AVSP-IDDISTR                  
           MOVE WS-IDKUNDNR      (RADIND)    TO   AVSP-IDKUNDNR                 
           MOVE WS-IDORDNR       (RADIND)    TO   AVSP-IDORDNR                  
           MOVE WS-IDORDER       (RADIND)    TO   AVSP-IDORDER                  
           MOVE WS-IDPRODNR      (RADIND)    TO   AVSP-IDPRODNR                 
           MOVE WS-IDPLKLST      (RADIND)    TO   AVSP-IDPLKLST                 
           MOVE WS-IDKOLLI       (RADIND)    TO   AVSP-IDKOLLI                  
           MOVE WS-KDKOLLI       (RADIND)    TO   AVSP-KDKOLLI                  
           MOVE WS-KDEMBTYP      (RADIND)    TO   AVSP-KDEMBTYP                 
           MOVE WS-VKORDBTO      (RADIND)    TO   AVSP-VKORDBTO                 
                                                                                
      *    IF WS-KDKOLLI (RADIND) > ZERO                                        
      *      MOVE WS-KDKOLLI (RADIND)        TO   AVSP-KDKOLLI                  
      *    ELSE                                                                 
      *      MOVE REQU-KDKOLLI (RADIND)      TO   AVSP-KDKOLLI                  
      *    END-IF                                                               
           IF WS-KDEMBTYP (RADIND) > SPACE                                      
             MOVE WS-KDEMBTYP  (RADIND)      TO   AVSP-KDEMBTYP                 
           ELSE                                                                 
             MOVE REQU-KDEMBTYP  (RADIND)    TO   AVSP-KDEMBTYP                 
           END-IF                                                               
                                                                                
      *    IF REQU-DIKOLLIL (RADIND) NUMERIC                                    
      *      AND REQU-DIKOLLIL (RADIND) > ZERO                                  
      *       MOVE REQU-DIKOLLIL (RADIND)    TO   AVSP-DIKOLLIL                 
      *    ELSE                                                                 
              MOVE WS-DIKOLLIL (RADIND)      TO   AVSP-DIKOLLIL                 
      *    END-IF                                                               
                                                                                
      *    IF REQU-DIKOLLIB (RADIND) NUMERIC                                    
      *      AND REQU-DIKOLLIB (RADIND) > ZERO                                  
      *       MOVE REQU-DIKOLLIB (RADIND)    TO   AVSP-DIKOLLIB                 
      *    ELSE                                                                 
              MOVE WS-DIKOLLIB  (RADIND)     TO   AVSP-DIKOLLIB                 
      *    END-IF                                                               
                                                                                
      *    IF REQU-DIKOLLIH (RADIND) NUMERIC                                    
      *      AND REQU-DIKOLLIH (RADIND) > ZERO                                  
      *       MOVE REQU-DIKOLLIH (RADIND)    TO   AVSP-DIKOLLIH                 
      *    ELSE                                                                 
              MOVE WS-DIKOLLIH  (RADIND)     TO   AVSP-DIKOLLIH                 
      *    END-IF                                                               
                                                                                
           MOVE WS-KDKOLLID      (RADIND)    TO   AVSP-KDKOLLID                 
           MOVE WS-KORD-VKORDNTO (RADIND)    TO   AVSP-VKORDNTO                 
           MOVE WS-VKTARA        (RADIND)    TO   AVSP-VKTARA                   
           .                                                                    
           SKIP2                                                                
       DB-AVSP-CALL          SECTION.                                           
           MOVE 'STA DB-AVSP-'         TO ERROR-TEXT                            
                                                                                
           CALL W403AVSP USING AVSP-W403AVSP                                    
           AVSP-4342-PCB  AVSP-2191-PCB   AVSP-4349-PCB                         
           TMS-CRE-PCB TMS-DEL-PCB                                              
           ATAB-PCB       AVSP-USEA-PCB                                         
           AVSP-WDE41-PCB AVSP-WDE42-PCB AVSP-WDE4-PCB AVSP-WDE4B-PCB           
           AVSP-WDE4E-PCB AVSP-WDE6-PCB  AVSP-XXDV-PCB  AVSP-ORQA-PCB           
           AVSP-XXKW-PCB  AVSP-XXLB-PCB  AVSP-XXJK-PCB AVSP-ZZAC-PCB            
           AVSP-ORQI-PCB  AVSP-ORQL-PCB  AVSP-WDE6E-PCB AVSP-WDE62-PCB          
           AVSP-ORQICSQ-PCB              AVSP-ORQM-PCB                          
           AVSP-WDE4A-PCB AVSP-WDM2-PCB  AVSP-ORQA2-PCB                         
           AVSP-ORDP1-PCB AVSP-ORDP2-PCB AVSP-XXJN-PCB AVSP-XXKH-PCB            
           AVSP-ARTC-PCB  AVSP-WDK7-PCB  AVSP-ARTM-PCB AVSP-AUTF-PCB            
           AVSP-4487-PCB  AVSP-4541-PCB AVSP-LOGA-PCB                           
           AVSP-WDK6-PCB  AVSP-WDA6B-PCB AVSP-WDA6-PCB AVSP-WDP4A-PCB           
           AVSP-WDB6-PCB                                                        
           AVSP-PLATS-XXDM-PCB AVSP-PLATS-XXDN-PCB AVSP-PLATS-XXDP-PCB          
           AVSP-PLATS-XXDO-PCB AVSP-PLATS-WDE6C-PCB AVSP-PLATS-GMTC-PCB         
           AVSP-PLATS-WDB6-PCB                                                  
           AVSP-DNOT-ORQP-PCB                                                   
           AVSP-DNOT-ORQP2-PCB                                                  
           AVSP-DNOT-ORQP3-PCB                                                  
           AVSP-DNOT-4013-PCB                                                   
           AVSP-DNOT-BENA-PCB                                                   
           AVSP-PRQU-WDG2-PCB                                                   
           AVSP-PRQU-WDC7-PCB                                                   
           AVSP-PRQU-SJKO-WDK6-PCB                                              
           AVSP-PRNO-3107-PCB                                                   
           AVSP-TMS-1165-PCB                                                    
           AVSP-TMS-4141-PCB                                                    
           AVSP-TMS-WDB2-PCB                                                    
           AVSP-TMS-WDB6-PCB                                                    
           AVSP-TMS-WDD3-PCB                                                    
           AVSP-TMS-WDB1-PCB                                                    
           AVSP-TMS-WDE4A-PCB                                                   
           AVSP-TMS-WDE4F-PCB                                                   
           AVSP-TMS-WDQ2-PCB                                                    
           AVSP-TMS-WDQ3-PCB                                                    
           AVSP-TMS-WDK6-PCB                                                    
           AVSP-TMS-WDE6-PCB                                                    
           AVSP-TMS-WDK5-PCB                                                    
           AVSP-TMS-WDQ2C-PCB                                                   
           .                                                                    
           SKIP2                                                                
       DC-ERROR-MESSAGE-TO-MOD      SECTION.                                    
           MOVE 'STA DC-ERROR'         TO ERROR-TEXT                            
                                                                                
           EVALUATE AVSP-ERROR-MESSAGE                                          
              WHEN 730                                                          
                   MOVE ERR-ERROR-IN-ADDRESS   TO RESP-IDMSG-ERROR              
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
              WHEN 317                                                          
                   MOVE ERR-MIXED-CASE-ON-OTHER-TRANS                           
                        TO RESP-IDMSG-ERROR                                     
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
              WHEN 318                                                          
                   MOVE ERR-MIXED-CASE-HAS-NO-TRANS                             
                        TO RESP-IDMSG-ERROR                                     
                                       RESP-IDMSG-ERROR-RAD (RADIND)            
              WHEN OTHER                                                        
                   MOVE ERR-ORDER-NOT-PACKED   TO RESP-IDMSG-ERROR              
      *ERR-ORDER-NOT-PACKED=021 GER FELMEDDELANDE                               
      *'ERRORS ON MARKED LINES' IN W.PROD.HTML.WZ01MSEN                         
           END-EVALUATE                                                         
                                                                                
           MOVE NEJ              TO INDATA-SW                                   
           .                                                                    
           SKIP2                                                                
       DE-CHECK-IF-INDATA-EXISTS   SECTION.                                     
           MOVE 'STA DE-CHECK-'       TO ERROR-TEXT                             
                                                                                
           MOVE NEJ                     TO INDATA-FINNS-SW                      
           MOVE +1                      TO RADIX                                
           PERFORM UNTIL RADIX > MAX-RADINDX                                    
                                                                                
             IF REQU-FLAVSP(RADIX) = FEL-FR-W403AVSP                            
               MOVE NEJ                 TO INDATA-SW                            
               MOVE 'IDPRODNR'       TO RESP-IDELMT-ERROR                       
               MOVE '023'            TO RESP-IDMSG-ERROR                        
                                        RESP-IDMSG-ERROR-RAD (RADIX)            
             END-IF                                                             
                                                                                
             IF  REQU-RAD    (RADIX) NOT = ALL '+'                              
             AND REQU-FLAVSP (RADIX)    = '+'                                   
               MOVE JA                  TO INDATA-FINNS-SW                      
             END-IF                                                             
             ADD +1                     TO RADIX                                
           END-PERFORM                                                          
           .                                                                    
                                                                                
                                                                                
       DF-LADDA-L198       SECTION.                                             
           MOVE 'STA DF-LADD'   TO ERROR-TEXT                                   
                                                                                
                                                                                
           MOVE WS-IDANSTNR        (RADIND)  TO RESP-L198-IDANSTNR-KEY          
           MOVE WS-IDDISTR         (RADIND)  TO RESP-L198-IDDISTR-KEY           
           MOVE WS-IDKUNDNR        (RADIND)  TO RESP-L198-IDKUNDNR-KEY          
           MOVE WS-IDORDNR         (RADIND)  TO RESP-L198-IDORDNR-KEY           
           MOVE WS-IDPRODNR        (RADIND)  TO RESP-L198-IDPRODNR-KEY          
           MOVE REQU-IDDC-KEY                TO RESP-L198-IDDC-KEY              
           MOVE WS-IDKOLLI         (RADIND)  TO RESP-L198-IDKOLLI-KEY           
                                                                                
           MOVE +1                           TO INDX                            
           PERFORM UNTIL INDX > +15                                             
             MOVE ALL-PLUS                   TO RESP-L198-RAD (INDX)            
             ADD +1                          TO INDX                            
           END-PERFORM                                                          
           .                                                                    
                                                                                
       E-READ-AGAIN-DB        SECTION.                                          
           MOVE 'STA E-READ-AGAIN'   TO ERROR-TEXT                              
                                                                                
           MOVE +1               TO RADIND                                      
                                                                                
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
                                                                                
             IF  REQU-RAD      (RADIND) = ALL '+'                               
               MOVE ZERO               TO WS-IDPRODNR(RADIND)                   
               MOVE ZERO               TO WS-IDKOLLI (RADIND)                   
               MOVE ZERO               TO WS-IDDISTR  (RADIND)                  
               MOVE ZERO               TO WS-IDKUNDNR (RADIND)                  
               MOVE ZERO               TO WS-IDORDNR (RADIND)                   
               MOVE ZERO               TO WS-IDUSER  (RADIND)                   
             ELSE                                                               
               MOVE REQU-IDPRODNR (RADIND) TO WS-IDPRODNR(RADIND)               
               MOVE REQU-IDPRODNR (RADIND) TO W-IDPRODNR                        
                                              W-IDPRODNR-WDQ3D                  
               MOVE REQU-IDKOLLI (RADIND)  TO WS-IDKOLLI (RADIND)               
                                                                                
               PERFORM IMS-GU-WDE601                                            
               IF SEGMENT-FINNS                                                 
                 MOVE VORD-IDDISTR     TO WS-IDDISTR  (RADIND)                  
                 MOVE VORD-IDKUNDNR    TO WS-IDKUNDNR (RADIND)                  
               ELSE                                                             
                 MOVE ZERO             TO WS-IDDISTR  (RADIND)                  
                 MOVE ZERO             TO WS-IDKUNDNR (RADIND)                  
               END-IF                                                           
                                                                                
               MOVE REQU-IDPLKLST(RADIND)  TO W-IDPLKLST-WDQ3D                  
               PERFORM IMS-GU-WDQ3D1                                            
               IF SEGMENT-FINNS                                                 
                 MOVE ODEL-IDORDNR7    TO WS-IDORDNR (RADIND)                   
                 MOVE ODEL-IDUSER      TO WS-IDUSER  (RADIND)                   
               ELSE                                                             
                 MOVE ZERO             TO WS-IDORDNR (RADIND)                   
                 MOVE ZERO             TO WS-IDUSER  (RADIND)                   
               END-IF                                                           
             END-IF                                                             
                                                                                
             ADD +1                TO RADIND                                    
           END-PERFORM                                                          
                                                                                
           .                                                                    
           EJECT                                                                
       F-CHECK-PRINT-OF-LISTS          SECTION.                                 
           MOVE 'STA F-CHECK-PRIN'       TO ERROR-TEXT                          
           PERFORM S04-EV-SEND-CASELABEL                                        
                                                                                
           IF REQU-FLSKRIV-DELNOTE = YES                                        
              PERFORM S12-SEND-DEL-NOTE                                         
           END-IF                                                               
           MOVE REQU-FLSKRIV-CLABEL  TO RESP-FLSKRIV-CLABEL                     
           MOVE REQU-FLSKRIV-DELNOTE TO RESP-FLSKRIV-DELNOTE                    
           .                                                                    
           SKIP2                                                                
       G-SET-KVRADER-NO-OF-LINES            SECTION.                            
           MOVE 'STA G-SET-KVRADE'       TO ERROR-TEXT                          
           MOVE +1          TO RADIND                                           
           MOVE ZERO        TO WS-KVRADER                                       
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
             IF WS-IDPRODNR (RADIND) > ZERO                                     
               ADD +1       TO WS-KVRADER                                       
             END-IF                                                             
             ADD +1          TO RADIND                                          
           END-PERFORM                                                          
                                                                                
           IF WS-KVRADER > ZERO                                                 
             MOVE WS-KVRADER           TO      RESP-L128-KVRADER                
             MOVE WS-KVRADER           TO      RESP-L129-KVRADER                
           ELSE                                                                 
             MOVE ZERO                 TO      RESP-L128-KVRADER                
             MOVE ZERO                 TO      RESP-L129-KVRADER                
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       S04-EV-SEND-CASELABEL  SECTION.                                          
           MOVE 'STA S04-EV-SEND-'       TO ERROR-TEXT                          
                                                                                
           IF REQU-FLSKRIV-CLABEL = YES                                         
             PERFORM S05-SEND-PRINTTRANS                                        
           END-IF                                                               
           .                                                                    
           SKIP2                                                                
       S05-SEND-PRINTTRANS          SECTION.                                    
           MOVE 'STA S05-SEND-'       TO ERROR-TEXT                             
                                                                                
           MOVE +1          TO RADIND                                           
           MOVE +1          TO INDX                                             
                                                                                
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
             IF REQU-FLAVSP (RADIND) = 'R'                                      
               IF WS-IDPRODNR (RADIND) > ZERO                                   
                 MOVE REQU-IDDC-KEY TO RESP-L128-IDDC-KEY (INDX)                
                 MOVE WS-IDDISTR (RADIND) TO WS-IDDISTR-NUM4                    
                 MOVE WS-IDDISTR-NUM4 TO RESP-L128-IDDISTR-KEY(INDX)            
                 MOVE WS-IDKUNDNR (RADIND) TO WS-IDKUNDNR-NUM6                  
                 MOVE WS-IDKUNDNR-NUM6 TO RESP-L128-IDKUNDNR-KEY(INDX)          
                 MOVE WS-IDORDNR (RADIND)                                       
                                     TO RESP-L128-IDORDNR-KEY (INDX)            
                 IF WS-IDKOLLI (RADIND) NUMERIC                                 
                   MOVE WS-IDKOLLI (RADIND)                                     
                                    TO RESP-L128-IDKOLLI-KEY (INDX)             
                 END-IF                                                         
                 MOVE WS-IDPRODNR(RADIND)                                       
                                    TO RESP-L128-IDPRODNR-KEY (INDX)            
                 MOVE '00000'  TO RESP-L128-IDKOLLI-TOM (INDX)                  
                 ADD +1  TO INDX                                                
               END-IF                                                           
             END-IF                                                             
             ADD +1      TO RADIND                                              
           END-PERFORM                                                          
           .                                                                    
           EJECT                                                                
       S12-SEND-DEL-NOTE         SECTION.                                       
           MOVE 'STA S12-SEND-DEL-'    TO ERROR-TEXT                            
      *SECTIONEN SÄNDER FÖLJESEDEL-TRANS TILL SKRIVARE.                         
           MOVE +1      TO RADIND                                               
           MOVE +1      TO INDX                                                 
           MOVE 'Y'                    TO RESP-L129-FLBG                        
                                                                                
           PERFORM UNTIL RADIND > MAX-RADINDX                                   
             IF REQU-FLAVSP (RADIND) = 'R'                                      
               IF WS-IDPRODNR (RADIND) > ZERO                                   
                 MOVE REQU-IDDC-KEY       TO RESP-L129-IDDC-KEY (INDX)          
                 MOVE WS-IDDISTR (RADIND) TO WS-IDDISTR-NUM4                    
                 MOVE WS-IDDISTR-NUM4     TO                                    
                                            RESP-L129-IDDISTR-KEY(INDX)         
                 MOVE WS-IDKUNDNR (RADIND) TO WS-IDKUNDNR-NUM6                  
                 MOVE WS-IDKUNDNR-NUM6    TO                                    
                                            RESP-L129-IDKUNDNR-KEY(INDX)        
                 MOVE WS-IDORDNR (RADIND) TO                                    
                                            RESP-L129-IDORDNR-KEY (INDX)        
                 IF WS-IDKOLLI (RADIND) NUMERIC                                 
                   MOVE WS-IDKOLLI (RADIND) TO                                  
                                            RESP-L129-IDKOLLI-KEY(INDX)         
                 END-IF                                                         
                 MOVE REQU-FLSKRIV-DELNOTE TO                                   
                                        RESP-L129-FLSKRIV-DELNOTE (INDX)        
                 ADD +1  TO INDX                                                
               END-IF                                                           
             END-IF                                                             
             ADD +1      TO RADIND                                              
           END-PERFORM                                                          
           .                                                                    
           SKIP2                                                                
       S15-RENSA-RAD SECTION.                                                   
           MOVE 'STA S15-RENSA-'    TO ERROR-TEXT                               
      *RENSA RESPONSE-RAD:                                                      
           MOVE ZERO               TO RESP-IDPRODNR      (RADIND)               
                                      RESP-IDPLKLST      (RADIND)               
                                      RESP-IDKOLLI       (RADIND)               
                                      RESP-IDKOLLI       (RADIND)               
                                      RESP-VKORDBTO-KOLLI (RADIND)              
                                      RESP-KDEMBTYP      (RADIND)               
                                      RESP-DIKOLLIL      (RADIND)               
                                      RESP-DIKOLLIB      (RADIND)               
                                      RESP-DIKOLLIH      (RADIND)               
           MOVE SPACE              TO RESP-KDKOLLI       (RADIND)               
                                      RESP-FLAVSP        (RADIND)               
                                      RESP-IDMSG-ERROR-RAD (RADIND)             
           .                                                                    
           SKIP2                                                                
      *    --- DISPATCHER SECTIONS                                              
       S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
           MOVE 'STA S01-FETCH-REQUEST'    TO ERROR-TEXT                        
                                                                                
           MOVE 'GETARG'               TO SUB-KDFUNC                            
           MOVE WS-ABSTRACT-ADRESS     TO SUB-ADDISPABS                         
           MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
                                                                                
           CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
                                                                                
           IF SUB-KDRC > 0                                                      
             MOVE SUB-KDRC TO KDRC-DISPLAY                                      
             STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
             DELIMITED BY SIZE INTO ERROR-TEXT                                  
             CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
           END-IF                                                               
           .                                                                    
           SKIP3                                                                
       S02-RETURN-RESPONSE SECTION.                                             
           MOVE 'STA S02-RETURN-RESPONSE'    TO ERROR-TEXT                      
                                                                                
           MOVE 'RETURN'                   TO SUB-KDFUNC                        
           MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
                                                                                
           CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
                                                                                
           IF SUB-KDRC > 0                                                      
             MOVE SUB-KDRC TO KDRC-DISPLAY                                      
             STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
             DELIMITED BY SIZE INTO ERROR-TEXT                                  
             CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
           END-IF                                                               
           .                                                                    
           EJECT                                                                
      *IMS SEKTIONER                                                            
      *                                                                         
      *                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
      *                 III     III MM MMMMM MM SSSS   SSSS                     
      *                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
      *                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
      *                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
      *                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
      *                 III     III MM MMMMM MM SSSS   SSSS                     
      *                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
      *                                                                         
      *                                                                         
      *IMS-SEKTIONER FRÅN 0129                                                  
       IMS-GU-EMBB SECTION.                                                     
           MOVE 'STA IMS-GU-EMBB   '  TO ERROR-TEXT                             
           STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-WDK5 ')'                      
                  DELIMITED BY SIZE INTO SSA1                                   
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU EMBB-PCB EMB-WKOLLI SSA1                       
           MOVE EMBB-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
       IMS-GU-WDE601 SECTION.                                                   
           MOVE 'STA IMS-GU-WDE601 '  TO ERROR-TEXT                             
           STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
                  DELIMITED BY SIZE INTO SSA1                                   
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDE6-PCB VORD-WDE601 SSA1                      
           MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
                                                                                
           .                                                                    
       IMS-GNP-WDE611 SECTION.                                                  
           MOVE 'STA IMS-GNP-WDE611 ' TO ERROR-TEXT                             
           STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
                  DELIMITED BY SIZE INTO SSA1                                   
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GNP WDE6-PCB KOLLI-WDE611 SSA1                    
           MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP2                                                                
       IMS-GU-WDE401      SECTION.                                              
           MOVE 'STA IMS-GU-WDE401  ' TO ERROR-TEXT                             
           STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
                  DELIMITED BY SIZE INTO SSA1                                   
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU  WDE4-PCB KORD-WDE401 SSA1                     
           MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP2                                                                
       IMS-GNP-WDE4        SECTION.                                             
           MOVE 'STA IMS-GNP-WDE4   ' TO ERROR-TEXT                             
           STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
                  DELIMITED BY SIZE INTO SSA1                                   
           STRING 'WDE411  *F(IDPURAD  >' W-WDE411-IDPURAD-X ')'                
                  DELIMITED BY SIZE INTO SSA2                                   
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GNP WDE4-PCB ORAD-WDE411 SSA1 SSA2                
           MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP2                                                                
       IMS-GNP-WDE411          SECTION.                                         
           MOVE 'STA IMS-GNP-WDE411 ' TO ERROR-TEXT                             
           MOVE   'WDE411   '          TO SSA1                                  
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GNP WDE4-PCB ORAD-WDE411 SSA1                     
           MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           SKIP3                                                                
           .                                                                    
       IMS-GU-WDQ3D1 SECTION.                                                   
           MOVE 'STA IMS-GU-WDQ3D1  ' TO ERROR-TEXT                             
           STRING 'WLORQA01(WDQ3DSEQ =' W-WDQ3D-X ')'                           
                  DELIMITED BY SIZE INTO SSA1                                   
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU  ORQA-PCB ODEL-WDQ301 SSA1                     
           MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
                                                                                
       IMS-PURG-TRANS4349 SECTION.                                              
           MOVE SPACE                TO GODK-STATUSKODER                        
           CALL CBLTDLI USING PURG AVSP-4349-PCB                                
           MOVE AVSP-4349-STATUS-CODE   TO STATUS-WS                            
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
       IMS-STATUSKONTROLL SECTION.                                              
           SET STATUS-IX TO 1                                                   
           SEARCH GODK-STATUS AT END CALL FELLOG                                
             WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
           END-SEARCH                                                           
           .                                                                    
