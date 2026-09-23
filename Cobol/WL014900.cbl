001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WL014900.                                                
001400 AUTHOR.         SUBBARAO PARUCHURI V.                                    
001500 DATE-WRITTEN.   04/07/07.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    NAME:       'CARPARTS.LDC.SHOWCASEQUEUE'                             
001900*                                                                         
002000*    FUNCTION:                                                            
002100*        FUNKTION:                                                        
002200*        VISAR KOLLIKÖ                                                    
002300*        INLÄGGNING AV HELT KOLLI ELLER VAL AV KOLLI FÖR                  
002400*        VIDARE                                                           
002500*        BEHANDLING                                                       
002600*        PROGRAMMET UPPDATERAR WLRETA (WDA3)                              
002700*        PROGRAMMET LÄSER      WLRETB (WDA3)                              
002800*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
002900*        SUB PROGRAMMET W006KOM  UPPDATERAR WLKOMA (WDP8)                 
003000*                                                                         
003200*        WL014900 PROGRAM IS A REPLICA OF W4073400 PROGRAM                
003210*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
003220*                                                                         
003300*    INDATA.                                                              
003400*        TRANSACTION: WL0149U                                             
003500*        REQUEST:     WL0149I1                                            
003600*                                                                         
003700*    OUTDATA.                                                             
003800*        RESPONSE:    WL0149O1                                            
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP2                                                                
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000     SKIP3                                                                
005100 FILE SECTION.                                                            
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500 77  IDPGM                       PIC X(08)   VALUE 'WL014900'.            
005600                                                                          
005700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005900 77  KDRC-DISPLAY                PIC Z(5).                                
005910 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005920                                                                          
005930 77  JA                          PIC X       VALUE 'J'.                   
005940 77  YES                         PIC X       VALUE 'Y'.                   
005950 77  NEJ                         PIC X       VALUE 'N'.                   
005960 77  W-CDC                       PIC X(3)    VALUE 'CDC'.                 
005970                                                                          
005980*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005990 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005991 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
005992 77  4792-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
005993 77  4792-MAX-INDX               PIC S9(4)  VALUE +24   COMP SYNC.        
005994 77  4797-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
005995 77  4797-MAX-INDX               PIC S9(4)  VALUE +16   COMP SYNC.        
005996 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005997 77  WS-COUNT                    PIC 9(3)   VALUE ZERO.                   
005998 77  WS-REC-LIMIT                PIC X      VALUE 'N'.                    
005999     88  REC-LIMIT                          VALUE 'J'.                    
006000 77  WS-INDX-REC                 PIC S9(4)  VALUE ZERO COMP SYNC.         
006001 77  WS-IDELMT-ERROR             PIC X(16)  VALUE SPACE.                  
006002 77  WS-IDMSG-ERROR              PIC X(03)  VALUE SPACE.                  
006003 77  WS-IDMSG-INFO               PIC X(03)  VALUE SPACE.                  
006004 77  SW-IDANSV                   PIC X      VALUE 'N'.                    
006005 77  SW-IDRT                     PIC X      VALUE 'N'.                    
006006 77  SW-IDDISTR                  PIC X      VALUE 'N'.                    
006007 77  SW-IDKOLLI                  PIC X      VALUE 'N'.                    
006008                                                                          
006009 77  W-UPDATE-SW                 PIC X       VALUE 'N'.                   
006010     88  W-UPDATE-OK                         VALUE 'J'.                   
006011                                                                          
006012*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006013 01  WS-IDANSV                   PIC X(4)   VALUE SPACE.                  
006014 01  WS-IDRETSND.                                                         
006015   03 WS-IDRT                    PIC X(3)   VALUE SPACE.                  
006020   03 WS-IDRTLOP                 PIC X(3)   VALUE SPACE.                  
006030 01  WS-IDKOLLI                  PIC X(5)   VALUE SPACE.                  
006040 01  WS-IDDISTR                  PIC X(5)   VALUE SPACE.                  
006050 01  WS-FLVISAAV                 PIC X(1)   VALUE SPACE.                  
006060                                                                          
006070 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006080     88  INDATA-OK                           VALUE 'J'.                   
006090     88  INDATA-FEL                          VALUE 'N'.                   
006091                                                                          
006092 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006093     88  NYCKLAR-OK                          VALUE 'J'.                   
006094     88  NYCKLAR-FEL                         VALUE 'N'.                   
006100     EJECT                                                                
006110*    --- KONSTANTER                                                       
006120 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
006130*                                                                         
006170 77  W-BINNED                    PIC  X(3)  VALUE 'BIN'.                  
006180 77  W-SELECTCASE                PIC  X(3)  VALUE 'SC '.                  
006190 77  W-SND-SAENT                 PIC X(1)   VALUE '2'.                    
006191 77  W-SND-LOSS                  PIC X(1)   VALUE '3'.                    
006192 77  W-SND-MOT                   PIC X(1)   VALUE '4'.                    
006193 77  W-SND-PAAB                  PIC X(1)   VALUE '5'.                    
006194 77  W-SND-INL                   PIC X(1)   VALUE '6'.                    
006195 77  W-ANM-MOT                   PIC X(1)   VALUE '5'.                    
006196 77  W-ANM-PAAB                  PIC X(1)   VALUE '6'.                    
006197 77  W-KLI-LOSS                  PIC S9(1)  VALUE +4 COMP-3.              
006198 77  W-KLI-MOT                   PIC S9(1)  VALUE +5 COMP-3.              
006199 77  W-KLI-SAK                   PIC S9(1)  VALUE +6 COMP-3.              
006200 77  W-KLI-AVV                   PIC S9(1)  VALUE +7 COMP-3.              
006210 77  W-KLI-VALT                  PIC S9(1)  VALUE +8 COMP-3.              
006220 77  W-KLI-BEH                   PIC S9(1)  VALUE +9 COMP-3.              
006240                                                                          
006250 77  W-IDANSTNR                  PIC S9(5)  VALUE ZERO COMP-3.            
006290 77  W-KVRADER-KVAR              PIC S9(5)  VALUE ZERO COMP-3.            
006292 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006293 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
006294 77  W-SPAR-IDRT                 PIC X(3)    VALUE SPACE.                 
006295 77  W-SPAR-IDRTLOP              PIC 9(3)    VALUE ZERO.                  
006296 77  W-SPAR-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.           
006297                                                                          
006298 77  SW-SOEKNING                 PIC X       VALUE '0'.                   
006299     88  INGEN-SOEK                          VALUE '0'.                   
006300     88  IDANSV-SOEK                         VALUE '1'.                   
006310     88  IDRETSND-SOEK                       VALUE '2'.                   
006320     88  IDDISTR-SOEK                        VALUE '3'.                   
006330                                                                          
006340 77  SW-FOERSTA-VALDA            PIC X       VALUE 'J'.                   
006350     88  FOERSTA-VALDA-KOLLI                 VALUE 'J'.                   
006360                                                                          
006370 77  SW-KOLLI-VALT               PIC X       VALUE 'N'.                   
006380     88  KOLLI-VALT                          VALUE 'J'.                   
006390                                                                          
006391 77  SW-KOLLI-INL                PIC X       VALUE 'N'.                   
006392     88  KOLLI-INL                           VALUE 'J'.                   
006393                                                                          
006397 77  SW-AVVIKELSER               PIC X       VALUE 'N'.                   
006398     88  VISA-AVVIKELSER                     VALUE 'J'.                   
006399     88  VISA-EJ-AVVIKELSER                  VALUE 'N'.                   
006400                                                                          
006410                                                                          
006420*    --- DATUM                                                            
006430                                                                          
006440 01  W-TIAADDD.                                                           
006450     03  FILLER                  PIC 9(1)    VALUE ZERO.                  
006460     03  W-TIAA                  PIC 9(2)    VALUE ZERO.                  
006470     03  W-TIDDD                 PIC 9(3)    VALUE ZERO.                  
006480                                                                          
006490 01  W-TIAADDD-IDAG.                                                      
006491     03  W-TIAA-IDAG             PIC 9(2).                                
006492     03  W-TIDDD-IDAG            PIC 9(3).                                
006493                                                                          
006494 01  W-TIAAAAMMDD-KLIARB.                                                 
006495     03  W-TISEKEL-KLIARB            PIC 9(2).                            
006496     03  W-TIAAMMDD-KLIARB           PIC 9(6).                            
006497 01  W-TIAAAAMMDD-KLIAVV.                                                 
006498     03  W-TISEKEL-KLIAVV            PIC 9(2).                            
006499     03  W-TIAAMMDD-KLIAVV           PIC 9(6).                            
006500                                                                          
006600                                                                          
007300     EJECT                                                                
007400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007500 01  GENERAL-SUBPROGRAMS.                                                 
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007800     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007910     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
007930     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007960     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
007970     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008000     SKIP3                                                                
008100*    --- PARAMETERS TO ABEND                                              
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008700     SKIP3                                                                
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009800     SKIP3                                                                
009900*01  -COPY WZ01SUB                                                        
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010200     SKIP3                                                                
010300 01  REQU-AREA.                                                           
010400*    03  -COPY WZ01REQU                                                   
010500*    03  -COPY WL0149I1                                                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010800     SKIP3                                                                
010900 01  RESP-AREA.                                                           
011000*    03  -COPY WZ01RESP                                                   
011100*    03  -COPY WL0149O1                                                   
011200     EJECT                                                                
011300*    ---  LÄNKAREA TILL WDATKONV                                          
011310 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
011320                                                                          
011330*01 -COPY WDATAREA                                                        
011340     EJECT                                                                
011350*    ---  LÄNKAREA TILL W418OKOD                                          
011360 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
011370                                                                          
011380*01 -COPY W418OKOD           -PRE OKOD-.                                  
011390     EJECT                                                                
011391*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011392*01 -COPY WMEDAREA                                                        
011393     SKIP3                                                                
011394 01  MESSAGE-CODES.                                                       
011396     03  ERR-INFO-MISSING        PIC X(3)    VALUE '185'.                 
011398     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
011399     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
011402     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
011403     03  SYS-ERR                 PIC X(3)    VALUE '099'.                 
011404     03  MORE-LINES              PIC X(3)    VALUE '028'.                 
011415     EJECT                                                                
011416*                                                                         
011420*                                                                         
011425 01  BILD-HOPP-AREOR.                                                     
011435                                                                          
011436   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
011437   03      P-TO-P-SW.                                                     
011438                                                                          
011439     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
011440     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
011441     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
011442     05  P-TO-P-KDTRANS          PIC X(8).                                
011443     05  P-TO-P-IDTRANS          PIC X(4).                                
011444     05  P-TO-P-KDMFSFOR         PIC X(1).                                
011445     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
011446     EJECT                                                                
011447 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
011448     SKIP3                                                                
011449 01  KOM-MSG-IO-AREA.                                                     
011450*03  -COPY WMSGKOM                                                        
011451     EJECT                                                                
011452 01  FILLER                   PIC X(16)   VALUE 'MSG/KOM-AREA'.           
011453     SKIP2                                                                
011454*01  -COPY WMSGSNUF           -PRE P-TO-P-                                
011455                                                                          
011456     EJECT                                                                
011457 01      FILLER                  PIC X(24)   VALUE                        
011458                                 'MOD4792-MID-W4I79201'.                  
011459     SKIP2                                                                
011460     -COPY W4I79201 -PRE MOD4792-                                         
011461     EJECT                                                                
011462 01      FILLER                  PIC X(24)   VALUE                        
011463                                 'MOD4797-MID-W4I79701'.                  
011464     SKIP2                                                                
011465     -COPY W4I79701 -PRE MOD4797-                                         
011466     EJECT                                                                
011467 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011468     SKIP3                                                                
011470 01  NYCKLAR-TILL-BLAEDDRING.                                             
011480   02    W-MINKEY.                                                        
011490     03  W-MINKEY-IDTRANS         PIC  X(4)          VALUE '4734'.        
011491     03  W-MINKEY-IDSOEK          PIC  X(1)          VALUE ZERO.          
011492     03  W-MINKEY-SSA-ENTER       PIC  X(47).                             
011493     03  W-MINKEY-WDA3BSEQ REDEFINES W-MINKEY-SSA-ENTER.                  
011494         05  W-MINKEYB-IDDC       PIC  X(2).                              
011495         05  W-MINKEYB-IDRT       PIC  X(3).                              
011496         05  W-MINKEYB-IDRTLOP    PIC  9(3).                              
011497         05  W-MINKEYB-IDKOLLI    PIC S9(5)   COMP-3.                     
011498                                                                          
011499     03  W-MINKEY-WDA3FSEQ REDEFINES W-MINKEY-SSA-ENTER.                  
011500         05  W-MINKEYF-IDDC       PIC  X(2).                              
011510         05  W-MINKEYF-IDDISTR    PIC S9(5)   COMP-3.                     
011520         05  W-MINKEYF-IDKUNDNR   PIC S9(7)   COMP-3.                     
011530         05  W-MINKEYF-IDRAPPNR   PIC  9(7).                              
011540         05  W-MINKEYF-IDRT       PIC  X(3).                              
011550         05  W-MINKEYF-IDRTLOP    PIC  9(3).                              
011560         05  W-MINKEYF-IDKOLLI    PIC S9(5)   COMP-3.                     
011570                                                                          
011580     03  W-MINKEY-WDA3G1KY REDEFINES W-MINKEY-SSA-ENTER.                  
011590         05  W-MINKEYG1-IDDC       PIC  X(2).                             
011591         05  W-MINKEYG1-KDRETSTA   PIC  X(1).                             
011592         05  W-MINKEYG1-KDARBTYP   PIC  X(8).                             
011593         05  W-MINKEYG1-IDPERSON   PIC S9(3)   COMP-3.                    
011594         05  W-MINKEYG1-DARETANK   PIC  9(8).                             
011595         05  W-MINKEYG1-IDRT       PIC  X(3).                             
011596         05  W-MINKEYG1-IDRTLOP    PIC  9(3).                             
011597         05  W-MINKEYG1-IDKOLLI    PIC S9(5)   COMP-3.                    
011598         05  W-MINKEYG1-DAREGDAT   PIC  9(8).                             
011599         05  W-MINKEYG1-TIKLOCK    PIC S9(9)   COMP-3.                    
011600     03  W-MINKEY-SSA-NEXT        PIC  X(47).                             
011610     03  W-MINKEY-WDA3BSEQ REDEFINES W-MINKEY-SSA-NEXT.                   
011620         05  W-MINKEYB-IDDC-NEXT       PIC  X(2).                         
011630         05  W-MINKEYB-IDRT-NEXT       PIC  X(3).                         
011640         05  W-MINKEYB-IDRTLOP-NEXT    PIC  9(3).                         
011650         05  W-MINKEYB-IDKOLLI-NEXT    PIC S9(5)   COMP-3.                
011660                                                                          
011670     03  W-MINKEY-WDA3FSEQ REDEFINES W-MINKEY-SSA-NEXT.                   
011680         05  W-MINKEYF-IDDC-NEXT       PIC  X(2).                         
011690         05  W-MINKEYF-IDDISTR-NEXT    PIC S9(5)   COMP-3.                
011691         05  W-MINKEYF-IDKUNDNR-NEXT   PIC S9(7)   COMP-3.                
011692         05  W-MINKEYF-IDRAPPNR-NEXT   PIC  9(7).                         
011693         05  W-MINKEYF-IDRT-NEXT       PIC  X(3).                         
011694         05  W-MINKEYF-IDRTLOP-NEXT    PIC  9(3).                         
011695         05  W-MINKEYF-IDKOLLI-NEXT    PIC S9(5)   COMP-3.                
011696                                                                          
011697     03  W-MINKEY-WDA3G1KY REDEFINES W-MINKEY-SSA-NEXT.                   
011698         05  W-MINKEYG1-IDDC-NEXT      PIC  X(2).                         
011699         05  W-MINKEYG1-KDRETSTA-NEXT  PIC  X(1).                         
011700         05  W-MINKEYG1-KDARBTYP-NEXT  PIC  X(8).                         
011710         05  W-MINKEYG1-IDPERSON-NEXT  PIC S9(3)   COMP-3.                
011720         05  W-MINKEYG1-DARETANK-NEXT  PIC  9(8).                         
011730         05  W-MINKEYG1-IDRT-NEXT      PIC  X(3).                         
011740         05  W-MINKEYG1-IDRTLOP-NEXT   PIC  9(3).                         
011750         05  W-MINKEYG1-IDKOLLI-NEXT   PIC S9(5)   COMP-3.                
011760         05  W-MINKEYG1-DAREGDAT-NEXT  PIC  9(8).                         
011770         05  W-MINKEYG1-TIKLOCK-NEXT   PIC S9(9)   COMP-3.                
011780                                                                          
011790     SKIP3                                                                
011791 01  NYCKLAR-TILL-DLI.                                                    
011792                                                                          
011793     03  W-WDA301KY-X.                                                    
011794         05  W-IDDC                PIC  X(2)          VALUE SPACE.        
011795         05  W-DAREGDAT            PIC  9(8)          VALUE ZERO.         
011796         05  W-TIKLOCK             PIC S9(9)   COMP-3 VALUE ZERO.         
011797                                                                          
011798     03  W-WDA3BSEQ-MIN-X.                                                
011800         05  W-IDRT-BSEQ-MIN      PIC  X(3)          VALUE SPACE.         
011801         05  W-IDDC-BSEQ-MIN      PIC  X(2)          VALUE SPACE.         
011802         05  W-IDRTLOP-BSEQ-MIN   PIC  9(3)          VALUE ZERO.          
011803         05  W-IDKOLLI-BSEQ-MIN   PIC S9(5)   COMP-3 VALUE ZERO.          
011804                                                                          
011805     03  W-WDA3BSEQ-MAX-X.                                                
011807         05  W-IDRT-BSEQ-MAX      PIC  X(3)          VALUE SPACE.         
011808         05  W-IDDC-BSEQ-MAX      PIC  X(2)          VALUE SPACE.         
011809         05  W-IDRTLOP-BSEQ-MAX   PIC  9(3)          VALUE ZERO.          
011810         05  W-IDKOLLI-BSEQ-MAX   PIC S9(5)   COMP-3 VALUE ZERO.          
011811                                                                          
011812     03  W-WDA3G1KY-MIN-X.                                                
011813         05  W-IDDC-G1-MIN       PIC  X(2)          VALUE SPACE.          
011814         05  W-KDRETSTA-G1-MIN   PIC  X(1)          VALUE SPACE.          
011815         05  W-KDARBTYP-G1-MIN   PIC  X(8)          VALUE SPACE.          
011816         05  W-IDPERSON-G1-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
011817         05  W-DARETANK-G1-MIN   PIC  9(8)          VALUE ZERO.           
011818         05  W-IDRT-G1-MIN       PIC  X(3)          VALUE SPACE.          
011819         05  W-IDRTLOP-G1-MIN    PIC  9(3)          VALUE ZERO.           
011820         05  W-IDKOLLI-G1-MIN    PIC S9(5)   COMP-3 VALUE ZERO.           
011821         05  W-DAREGDAT-G1-MIN   PIC  9(8)          VALUE ZERO.           
011822         05  W-TIKLOCK-G1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
011823                                                                          
011824     03  W-WDA3G1KY-MAX-X.                                                
011825         05  W-IDDC-G1-MAX       PIC  X(2)          VALUE SPACE.          
011826         05  W-KDRETSTA-G1-MAX   PIC  X(1)          VALUE SPACE.          
011827         05  W-KDARBTYP-G1-MAX   PIC  X(8)          VALUE SPACE.          
011828         05  W-IDPERSON-G1-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
011829         05  W-DARETANK-G1-MAX   PIC  9(8)          VALUE ZERO.           
011830         05  W-IDRT-G1-MAX       PIC  X(3)          VALUE SPACE.          
011831         05  W-IDRTLOP-G1-MAX    PIC  9(3)          VALUE ZERO.           
011832         05  W-IDKOLLI-G1-MAX    PIC S9(5)   COMP-3 VALUE ZERO.           
011833         05  W-DAREGDAT-G1-MAX   PIC  9(8)          VALUE ZERO.           
011834         05  W-TIKLOCK-G1-MAX    PIC S9(9)   COMP-3 VALUE ZERO.           
011835                                                                          
011836     03  W-WDA3FSEQ-MIN-X.                                                
011837         05  W-IDDC-FSEQ-MIN       PIC  X(2)          VALUE SPACE.        
011838         05  W-IDDISTR-FSEQ-MIN    PIC S9(5)   COMP-3 VALUE ZERO.         
011839         05  W-IDKUNDNR-FSEQ-MIN   PIC S9(7)   COMP-3 VALUE ZERO.         
011840         05  W-IDRAPPNR-FSEQ-MIN   PIC  9(7)          VALUE ZERO.         
011841                                                                          
011842     03  W-WDA3FSEQ-MAX-X.                                                
011843         05  W-IDDC-FSEQ-MAX       PIC  X(2)          VALUE SPACE.        
011844         05  W-IDDISTR-FSEQ-MAX    PIC S9(5)   COMP-3 VALUE ZERO.         
011845         05  W-IDKUNDNR-FSEQ-MAX   PIC S9(7)   COMP-3 VALUE ZERO.         
011846         05  W-IDRAPPNR-FSEQ-MAX   PIC  9(7)          VALUE ZERO.         
011847                                                                          
011848     03  W-IDLEVANM-X.                                                    
011849         05  W-IDDISTR-ANM       PIC S9(5)    VALUE ZERO  COMP-3.         
011850         05  W-IDKUNDNR-ANM      PIC S9(7)    VALUE ZERO  COMP-3.         
011851         05  W-IDRAPPNR-ANM      PIC  9(7)    VALUE ZERO.                 
011852                                                                          
011853     03  W-WDA3F1KY-MIN-X.                                                
011854         05  W-IDDC-F1-MIN      PIC  X(2)          VALUE SPACE.           
011855         05  W-IDDISTR-F1-MIN   PIC S9(5)   COMP-3 VALUE ZERO.            
011856         05  W-IDKUNDNR-F1-MIN  PIC S9(7)   COMP-3 VALUE ZERO.            
011857         05  W-IDRAPPNR-F1-MIN  PIC  X(7)          VALUE ZERO.            
011858         05  W-IDRT-F1-MIN      PIC  X(3)          VALUE SPACE.           
011859         05  W-IDRTLOP-F1-MIN   PIC  9(3)          VALUE ZERO.            
011860         05  W-IDKOLLI-F1-MIN   PIC S9(5)   COMP-3 VALUE ZERO.            
011861         05  W-DAREGDAT-F1-MIN  PIC  9(8)          VALUE ZERO.            
011862         05  W-TIKLOCK-F1-MIN   PIC S9(9)   COMP-3 VALUE ZERO.            
011863                                                                          
011864     03  W-WDA3F1KY-MAX-X.                                                
011865         05  W-IDDC-F1-MAX      PIC  X(2)          VALUE SPACE.           
011866         05  W-IDDISTR-F1-MAX   PIC S9(5)   COMP-3 VALUE ZERO.            
011867         05  W-IDKUNDNR-F1-MAX  PIC S9(7)   COMP-3 VALUE ZERO.            
011868         05  W-IDRAPPNR-F1-MAX  PIC  X(7)          VALUE ZERO.            
011869         05  W-IDRT-F1-MAX      PIC  X(3)          VALUE SPACE.           
011870         05  W-IDRTLOP-F1-MAX   PIC  9(3)          VALUE ZERO.            
011871         05  W-IDKOLLI-F1-MAX   PIC S9(5)   COMP-3 VALUE ZERO.            
011872         05  W-DAREGDAT-F1-MAX  PIC  9(8)          VALUE ZERO.            
011873         05  W-TIKLOCK-F1-MAX   PIC S9(9)   COMP-3 VALUE ZERO.            
011874                                                                          
011875     03  W-KDARBTYP-X.                                                    
011876         05  W-KDARBTYP         PIC  X(8)          VALUE SPACE.           
011877                                                                          
011878     03  W-IDPERSON-X.                                                    
011879         05  W-IDPERSON         PIC S9(3)   COMP-3 VALUE ZERO.            
011880                                                                          
011881     03  W-WDGX4107-X.                                                    
011882         05  W-IDHTYP-4107      PIC X(4)          VALUE '4107'.           
011883         05  FILLER             PIC X(26)         VALUE LOW-VALUE.        
011884                                                                          
011885     03  W-KDSEGKEY-X.                                                    
011886         05  W-KDSEGKEY         PIC  X(1)          VALUE '1'.             
011887                                                                          
011888     SKIP2                                                                
011889*    --- STATUS-KOD FRÅN IMS                                              
011890 01  STATUS-WS                   PIC XX.                                  
011891     88  STATUS-OK                           VALUE '  '.                  
011892     88  SEGMENT-FINNS                       VALUE '  '.                  
011893     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011894     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011895     88  TRANSKOD-FEL                        VALUE 'A1'.                  
011896     88  SECURITY-FEL                        VALUE 'A4'.                  
011897     SKIP2                                                                
011898 01  GODK-STATUSKODER.                                                    
011899     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011900     SKIP3                                                                
011901 01  SSA1                        PIC X(256).                              
011902 01  SSA2                        PIC X(64).                               
011903     EJECT                                                                
011904*    --- IMS FUNKTIONSKODER                                               
011905*01  -COPY W0003                                                          
011906     EJECT                                                                
011907*    ---  DLI INPUT-OUTPUT AREA                                           
011908 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011909     SKIP3                                                                
011910 01  DLI-IO-AREA1.                                                        
011911     03  IO-AREA1                 PIC X(200)  VALUE SPACE.                
011912     SKIP3                                                                
011913     03  WLRETA01 REDEFINES IO-AREA1.                                     
011914*        05  -COPY WDA301                                                 
011915     EJECT                                                                
011916     03  WLRETG01 REDEFINES IO-AREA1.                                     
011917*        05  -COPY WDA3F1                                                 
011918     EJECT                                                                
011919 01  DLI-IO-AREA2.                                                        
011920     03  IO-AREA2                 PIC X(300)  VALUE SPACE.                
011921     SKIP3                                                                
011922     03  WLRETA01 REDEFINES IO-AREA2.                                     
011923*        05  -COPY WDA201                                                 
011924     EJECT                                                                
011925     03  WLRETA01 REDEFINES IO-AREA2.                                     
011926*        05  -COPY WDA211                                                 
011927     EJECT                                                                
011928     03  WLRETH01 REDEFINES IO-AREA2.                                     
011929*        05  -COPY WDA3G1                                                 
011930     EJECT                                                                
011931     03  WL410711 REDEFINES IO-AREA2.                                     
011932*        05  -COPY WDGX4108                                               
011933     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100 01  MSG-PCB                     PIC X.                                   
012300     EJECT                                                                
012310*01  -COPY W0009  -PRE DISP-                                              
012320      EJECT                                                               
012400*01  -COPY W0008  -PRE RETA1-                                             
012401     05  FILLER                  PIC X.                                   
012402     EJECT                                                                
012403*01  -COPY W0008  -PRE RETA2-                                             
012404     05  FILLER                  PIC X.                                   
012405                                                                          
012406     EJECT                                                                
012407*01  -COPY W0008  -PRE RETA3-                                             
012408     05  FILLER                  PIC X.                                   
012409     EJECT                                                                
012410*01  -COPY W0008  -PRE RETG-                                              
012411     05  FILLER                  PIC X.                                   
012412     EJECT                                                                
012413*01  -COPY W0008  -PRE RETH-                                              
012414     05  FILLER                  PIC X.                                   
012415     EJECT                                                                
012416*01  -COPY W0008  -PRE KREE-                                              
012417     05  FILLER                  PIC X.                                   
012424                                                                          
012425*01  -COPY W0008  -PRE 4107-                                              
012426     05  FILLER                  PIC X.                                   
012427                                                                          
012428 01  KOM-KOMA-PCB                PIC X.                                   
012430 PROCEDURE DIVISION  USING MSG-PCB   DISP-PCB RETA1-PCB                   
012431                          RETA2-PCB  RETA3-PCB RETG-PCB                   
012432                          RETH-PCB   KREE-PCB                             
012433                          4107-PCB   KOM-KOMA-PCB.                        
012435 MAIN SECTION.                                                            
012436     ENTRY 'DLITCBL' USING   MSG-PCB   DISP-PCB RETA1-PCB                 
012437                             RETA2-PCB RETA3-PCB RETG-PCB                 
012438                             RETH-PCB  KREE-PCB                           
012439                             4107-PCB  KOM-KOMA-PCB.                      
012440                                                                          
012500                                                                          
012700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012800     IF SUB-KDRC = 0                                                      
012900       IF REQU-KDPGMACT = 'S' OR 'E'                                      
012910         PERFORM A-INIT                                                   
012920         PERFORM B-KOLLA-NYCKLAR                                          
012930         IF NYCKLAR-OK                                                    
012940           IF REQU-KDPGMACT = 'E'                                         
012950             PERFORM G-KOLLA-INPUT                                        
012960             IF INDATA-OK                                                 
012970               PERFORM H-UPPDATERA                                        
012980             END-IF                                                       
013000           END-IF                                                         
013005           IF INDATA-OK                                                   
013007             PERFORM F-LAES-VISA-INFO                                     
013008           END-IF                                                         
013010         END-IF                                                           
013017       ELSE                                                               
013020         MOVE SYS-ERR      TO RESP-IDMSG-ERROR                            
013100       END-IF                                                             
013200       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
013300       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
013400       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
013500       IF WS-IDMSG-ERROR NOT = SPACE                                      
013600         MOVE ALL '+' TO RESP-WL0149O1(1:24)                              
013610         MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                        
013620         MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                       
013630         MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                         
013640         MOVE 001              TO RESP-IDMSGVER                           
013646         IF REQU-KDPGMACT = 'S'                                           
013647           MOVE ZERO             TO RESP-KVRADER                          
013648         ELSE                                                             
013649           IF REQU-KVRADER NUMERIC                                        
013650             MOVE REQU-KVRADER     TO RESP-KVRADER                        
013651           ELSE                                                           
013652             MOVE ZERO             TO RESP-KVRADER                        
013653           END-IF                                                         
013654         END-IF                                                           
013655       END-IF                                                             
013700       PERFORM S02-RETURN-RESPONSE                                        
013800     END-IF                                                               
014100                                                                          
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500     .                                                                    
014600     EJECT                                                                
014700 A-INIT SECTION.                                                          
014800                                                                          
017500     MOVE LOW-VALUE      TO W-WDA3BSEQ-MIN-X                              
017600                            W-WDA3G1KY-MIN-X                              
017700                            W-WDA3FSEQ-MIN-X                              
017800                                                                          
017900     MOVE HIGH-VALUE     TO W-WDA3BSEQ-MAX-X                              
018000                            W-WDA3G1KY-MAX-X                              
018100                            W-WDA3FSEQ-MAX-X                              
018200                                                                          
018300     MOVE '4'            TO W-KDRETSTA-G1-MIN                             
018400     MOVE '5'            TO W-KDRETSTA-G1-MAX                             
018410     MOVE ALL '+'        TO RESP-AREA                                     
018420     MOVE 001            TO RESP-IDMSGVER                                 
018421     MOVE ZERO           TO RESP-KVRADER                                  
018430     MOVE SPACE          TO RESP-IDMSG-ERROR                              
018440                            RESP-IDMSG-INFO                               
018450                            RESP-IDELMT-ERROR                             
018500     .                                                                    
018600     EJECT                                                                
018700 B-KOLLA-NYCKLAR SECTION.                                                 
020019                                                                          
020020     MOVE JA TO NYCKLAR-SW                                                
020021                                                                          
020022     MOVE REQU-IDDC-KEY       TO W-IDDC                                   
020023                                 W-IDDC-BSEQ-MIN                          
020024                                 W-IDDC-BSEQ-MAX                          
020025                                 W-IDDC-FSEQ-MIN                          
020026                                 W-IDDC-FSEQ-MAX                          
020027                                 W-IDDC-G1-MIN                            
020028                                 W-IDDC-G1-MAX                            
020029                                 W-IDDC-F1-MIN                            
020030                                 W-IDDC-F1-MAX                            
020032     PERFORM BA-KOLLA-IDANSV                                              
020033     PERFORM BB-KOLLA-IDRETSND                                            
020034     PERFORM BC-KOLLA-IDKOLLI                                             
020035     PERFORM BD-KOLLA-IDDISTR                                             
020036     PERFORM BE-KOLLA-FLVISAAV                                            
020037                                                                          
020038     IF INGEN-SOEK                                                        
020039       MOVE NEJ                   TO NYCKLAR-SW                           
020040       MOVE ERR-WRONG-KEY         TO RESP-IDMSG-ERROR                     
020041     END-IF                                                               
020042     IF SW-IDANSV = 'J' AND SW-IDRT = 'J'                                 
020043       MOVE NEJ                   TO NYCKLAR-SW                           
020044       MOVE '032'                 TO RESP-IDMSG-ERROR                     
020045     END-IF                                                               
020046     IF SW-IDANSV = 'J' AND SW-IDDISTR = 'J'                              
020047       MOVE NEJ                   TO NYCKLAR-SW                           
020048       MOVE '032'                 TO RESP-IDMSG-ERROR                     
020049     END-IF                                                               
020050     IF SW-IDANSV = 'J' AND SW-IDKOLLI = 'J'                              
020051       MOVE NEJ                   TO NYCKLAR-SW                           
020052       MOVE '032'                 TO RESP-IDMSG-ERROR                     
020053     END-IF                                                               
020054     IF SW-IDRT   = 'J' AND SW-IDDISTR = 'J'                              
020055       MOVE NEJ                   TO NYCKLAR-SW                           
020056       MOVE '032'                 TO RESP-IDMSG-ERROR                     
020057     END-IF                                                               
020058     IF SW-IDDISTR = 'J' AND SW-IDKOLLI = 'J'                             
020059       MOVE NEJ                   TO NYCKLAR-SW                           
020060       MOVE '032'                 TO RESP-IDMSG-ERROR                     
020061     END-IF                                                               
020062                                                                          
020063     MOVE REQU-IDANSV-KEY         TO RESP-IDANSV-KEY                      
020064     IF REQU-IDDISTR-KEY NUMERIC                                          
020065       MOVE REQU-IDDISTR-KEY      TO RESP-IDDISTR-KEY                     
020066       INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE           
020067     END-IF                                                               
020068     MOVE REQU-IDRT-KEY           TO RESP-IDRT-KEY                        
020069     IF REQU-IDRTLOP-KEY  NUMERIC                                         
020070       MOVE REQU-IDRTLOP-KEY      TO RESP-IDRTLOP-KEY                     
020071     END-IF                                                               
020072     IF REQU-IDKOLLI-KEY   NUMERIC                                        
020073       MOVE REQU-IDKOLLI-KEY      TO RESP-IDKOLLI-KEY                     
020074       INSPECT RESP-IDKOLLI-KEY REPLACING LEADING ZERO BY SPACE           
020075     END-IF                                                               
020076     MOVE REQU-IDDC-KEY           TO RESP-IDDC-KEY                        
020086                                                                          
020087     IF NYCKLAR-FEL                                                       
020088       MOVE ERR-WRONG-KEY         TO RESP-IDMSG-ERROR                     
020093     END-IF                                                               
020094     .                                                                    
020095     EJECT                                                                
020096                                                                          
020097 BA-KOLLA-IDANSV     SECTION.                                             
020098                                                                          
020116     IF REQU-IDANSV-KEY(1:3)        NOT = SPACE AND ALL '+'               
020117        IF REQU-IDANSV-KEY(4:3)     NUMERIC                               
020118           MOVE '1'                   TO SW-SOEKNING                      
020119           MOVE REQU-IDANSV-KEY(1:3)  TO  W-KDARBTYP                      
020120           MOVE REQU-IDANSV-KEY(4:3)  TO  W-IDPERSON                      
020121           MOVE JA                    TO SW-IDANSV                        
020122        ELSE                                                              
020123           MOVE NEJ                 TO NYCKLAR-SW                         
020124        END-IF                                                            
020125     END-IF                                                               
020126     .                                                                    
020127     EJECT                                                                
020129 BB-KOLLA-IDRETSND   SECTION.                                             
020130                                                                          
020138     IF REQU-IDRT-KEY          NOT = SPACE  AND ALL '+'                   
020140           IF REQU-IDRTLOP-KEY     NUMERIC                                
020141              MOVE '2'              TO SW-SOEKNING                        
020142              MOVE REQU-IDRT-KEY    TO W-IDRT-BSEQ-MIN                    
020143                                       W-IDRT-BSEQ-MAX                    
020144              MOVE REQU-IDRTLOP-KEY TO W-IDRTLOP-BSEQ-MIN                 
020145                                       W-IDRTLOP-BSEQ-MAX                 
020146              MOVE JA           TO SW-IDRT                                
020147           ELSE                                                           
020148              MOVE NEJ          TO NYCKLAR-SW                             
020150           END-IF                                                         
020151     END-IF                                                               
020152                                                                          
020153     .                                                                    
020154     EJECT                                                                
020155                                                                          
020156 BC-KOLLA-IDKOLLI    SECTION.                                             
020164                                                                          
020165     IF REQU-IDKOLLI-KEY          NUMERIC AND                             
020166        REQU-IDKOLLI-KEY          >  ZERO                                 
020167        IF IDRETSND-SOEK                                                  
020168           MOVE REQU-IDKOLLI-KEY  TO W-IDKOLLI-BSEQ-MIN                   
020169                                     W-IDKOLLI-BSEQ-MAX                   
020170           MOVE JA                TO SW-IDKOLLI                           
020171        ELSE                                                              
020172           MOVE NEJ               TO NYCKLAR-SW                           
020174        END-IF                                                            
020175     END-IF                                                               
020176                                                                          
020177     .                                                                    
020178     EJECT                                                                
020179                                                                          
020180 BD-KOLLA-IDDISTR    SECTION.                                             
020181                                                                          
020188     IF REQU-IDDISTR-KEY        NUMERIC AND                               
020189        REQU-IDDISTR-KEY        >  ZERO                                   
020191       MOVE '3'                  TO SW-SOEKNING                           
020192       MOVE REQU-IDDISTR-KEY TO W-IDDISTR-FSEQ-MIN                        
020193                                W-IDDISTR-FSEQ-MAX                        
020194       MOVE JA                   TO SW-IDDISTR                            
020197     END-IF                                                               
020198                                                                          
020199     .                                                                    
020200     EJECT                                                                
020201                                                                          
020202 BE-KOLLA-FLVISAAV   SECTION.                                             
020203                                                                          
020209     MOVE REQU-FLVISAAV-KEY  TO WS-FLVISAAV                               
020211                                                                          
020212     IF WS-FLVISAAV             =  JA                                     
020214       MOVE JA                  TO SW-AVVIKELSER                          
020215                                   RESP-FLVISAAV-KEY                      
020216     ELSE                                                                 
020217       IF WS-FLVISAAV           =  NEJ                                    
020218         MOVE NEJ               TO SW-AVVIKELSER                          
020219                                   RESP-FLVISAAV-KEY                      
020220       END-IF                                                             
020221     END-IF                                                               
020222                                                                          
020223     .                                                                    
020224     EJECT                                                                
020363                                                                          
020364 F-LAES-VISA-INFO SECTION.                                                
020365                                                                          
020366     PERFORM S05-BERAKNA-DATUM                                            
020367                                                                          
020368     MOVE ZERO                  TO W-SPAR-IDRT                            
020369                                   W-SPAR-IDRTLOP                         
020370                                   W-SPAR-IDKOLLI                         
020371     PERFORM FA-LAES-FOERSTA-POSTEN                                       
020372                                                                          
020373     IF SEGMENT-SAKNAS                                                    
020375       MOVE '025'                TO RESP-IDMSG-ERROR                      
020376       MOVE 'IDRT-IDRTLOP'       TO RESP-IDELMT-ERROR                     
020380     ELSE                                                                 
020381       MOVE +1                  TO INDX                                   
020382       PERFORM FB-FIXA-ENTER-KEY                                          
020383                                                                          
020384       PERFORM UNTIL INDX       > MAX-INDX                                
020385         IF SEGMENT-FINNS                                                 
020386                                                                          
020387           PERFORM FC-REDIGERA-MOD                                        
020388*   FÖR ATT INTE LÄSA VIDARE OM IDKOLLI ÄR IFYLLT                         
020389           IF IDRETSND-SOEK AND REQU-IDKOLLI-KEY NUMERIC AND              
020390                                REQU-IDKOLLI-KEY > ZERO                   
020391             MOVE 'GE'         TO STATUS-WS                               
020392           ELSE                                                           
020393*   FÖR ATT LÄSA NÄSTA KOLLI                                              
020394             MOVE RET-IDKOLLI  TO W-IDKOLLI-BSEQ-MIN                      
020395                                                                          
020396             PERFORM FD-LAES-NAESTA-POST                                  
020397           END-IF                                                         
020398         ELSE                                                             
020414           ADD 1                 TO INDX                                  
020415         END-IF                                                           
020416       END-PERFORM                                                        
020417                                                                          
020418       PERFORM FE-FIXA-NEXT-KEY                                           
020423                                                                          
020424     END-IF                                                               
020425     IF REQU-KDPGMACT = 'S' AND WS-COUNT = 0                              
020426       MOVE '025'                TO RESP-IDMSG-ERROR                      
020427       MOVE 'IDRT-IDRTLOP'       TO RESP-IDELMT-ERROR                     
020428     END-IF                                                               
020429     .                                                                    
020430     EJECT                                                                
020431                                                                          
020432 FA-LAES-FOERSTA-POSTEN    SECTION.                                       
020433                                                                          
020434     EVALUATE TRUE                                                        
020435                                                                          
020436       WHEN IDANSV-SOEK                                                   
020437        PERFORM IMS-GU-RETH-WLRETH01                                      
020438        IF SEGMENT-FINNS                                                  
020439           MOVE SEQG-DAREGDAT      TO W-DAREGDAT                          
020440           MOVE SEQG-TIKLOCK       TO W-TIKLOCK                           
020441                                                                          
020442           PERFORM IMS-GU-RETA-WLRETA01                                   
020443        END-IF                                                            
020444                                                                          
020445       WHEN IDRETSND-SOEK                                                 
020446        PERFORM IMS-GHU-SEQB-WLRETA01                                     
020447                                                                          
020448       WHEN IDDISTR-SOEK                                                  
020449        PERFORM IMS-GU-SEQF-WLRETA01                                      
020450        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
020451                      RET-IDKOLLI        > ZERO                           
020452           PERFORM IMS-GU-SEQF-WLRETA01                                   
020453        END-PERFORM                                                       
020454                                                                          
020455     END-EVALUATE                                                         
020456     .                                                                    
020457     EJECT                                                                
020458                                                                          
020459 FB-FIXA-ENTER-KEY        SECTION.                                        
020460                                                                          
020461     IF SEGMENT-FINNS                                                     
020462        EVALUATE TRUE                                                     
020463                                                                          
020464          WHEN IDANSV-SOEK                                                
020465           MOVE SEQG-IDDC           TO W-MINKEYG1-IDDC                    
020466           MOVE SEQG-KDRETSTA       TO W-MINKEYG1-KDRETSTA                
020467           MOVE SEQG-KDARBTYP       TO W-MINKEYG1-KDARBTYP                
020468           MOVE SEQG-IDPERSON       TO W-MINKEYG1-IDPERSON                
020469           MOVE SEQG-DARETANK       TO W-MINKEYG1-DARETANK                
020470           MOVE SEQG-IDRT           TO W-MINKEYG1-IDRT                    
020471           MOVE SEQG-IDRTLOP        TO W-MINKEYG1-IDRTLOP                 
020472           MOVE SEQG-IDKOLLI        TO W-MINKEYG1-IDKOLLI                 
020473           MOVE SEQG-DAREGDAT       TO W-MINKEYG1-DAREGDAT                
020474           MOVE SEQG-TIKLOCK        TO W-MINKEYG1-TIKLOCK                 
020475           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020476           MOVE '3'                 TO W-MINKEY-IDSOEK                    
020477                                                                          
020478          WHEN IDRETSND-SOEK                                              
020479           MOVE RET-IDDC            TO W-MINKEYB-IDDC                     
020480           MOVE RET-IDRT            TO W-MINKEYB-IDRT                     
020481           MOVE RET-IDRTLOP         TO W-MINKEYB-IDRTLOP                  
020482           MOVE RET-IDKOLLI         TO W-MINKEYB-IDKOLLI                  
020483           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020484           MOVE '1'                 TO W-MINKEY-IDSOEK                    
020485                                                                          
020486          WHEN IDDISTR-SOEK                                               
020487           MOVE RET-IDDC            TO W-MINKEYF-IDDC                     
020488           MOVE RET-IDDISTR         TO W-MINKEYF-IDDISTR                  
020489           MOVE RET-IDKUNDNR        TO W-MINKEYF-IDKUNDNR                 
020490           MOVE RET-IDRAPPNR        TO W-MINKEYF-IDRAPPNR                 
020491           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020492           MOVE '2'                 TO W-MINKEY-IDSOEK                    
020493                                                                          
020494        END-EVALUATE                                                      
020495     ELSE                                                                 
020496        EVALUATE TRUE                                                     
020497                                                                          
020498          WHEN IDANSV-SOEK                                                
020499           MOVE REQU-IDDC-KEY       TO W-MINKEYG1-IDDC                    
020500           MOVE W-SND-MOT           TO W-MINKEYG1-KDRETSTA                
020502           MOVE SPACE               TO W-MINKEYG1-IDRT                    
020503           MOVE ZERO                TO W-MINKEYG1-DARETANK                
020504                                       W-MINKEYG1-IDRTLOP                 
020505                                       W-MINKEYG1-IDKOLLI                 
020506                                       W-MINKEYG1-DAREGDAT                
020507                                       W-MINKEYG1-TIKLOCK                 
020508           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020509           MOVE '3'                 TO W-MINKEY-IDSOEK                    
020511                                                                          
020512          WHEN IDRETSND-SOEK                                              
020513           MOVE REQU-IDDC-KEY       TO W-MINKEYB-IDDC                     
020514           MOVE ZERO                TO W-MINKEYB-IDRT                     
020515                                       W-MINKEYB-IDRTLOP                  
020516                                       W-MINKEYB-IDKOLLI                  
020517           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020518           MOVE '1'                 TO W-MINKEY-IDSOEK                    
020520                                                                          
020521          WHEN IDDISTR-SOEK                                               
020522           MOVE REQU-IDDC-KEY       TO W-MINKEYF-IDDC                     
020523           MOVE REQU-IDDISTR-KEY    TO W-MINKEYF-IDDISTR                  
020524           MOVE ZERO                TO W-MINKEYF-IDKUNDNR                 
020525                                       W-MINKEYF-IDRAPPNR                 
020526           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020527           MOVE '2'                 TO W-MINKEY-IDSOEK                    
020529                                                                          
020530        END-EVALUATE                                                      
020531     END-IF                                                               
020532                                                                          
020533     .                                                                    
020534     EJECT                                                                
020535                                                                          
020536 FC-REDIGERA-MOD          SECTION.                                        
020537                                                                          
020538     IF RET-KDRETSTA < 6                                                  
020540       IF (VISA-AVVIKELSER    AND (RET-KDKOLSTA = W-KLI-SAK OR            
020541                                   RET-KDKOLSTA = W-KLI-AVV))  OR         
020542          (VISA-EJ-AVVIKELSER AND (RET-KDKOLSTA = W-KLI-LOSS OR           
020543                                   RET-KDKOLSTA = W-KLI-MOT))             
020544                                                                          
020546          ADD  +1              TO WS-COUNT                                
020547          IF REQU-KDPGMACT = 'S'                                          
020548            MOVE ZERO          TO RESP-IDANSTNR                           
020549          END-IF                                                          
020550          MOVE RET-IDRT        TO RESP-IDRT       (INDX)                  
020551                                  W-SPAR-IDRT                             
020552          MOVE RET-IDRTLOP     TO RESP-IDRTLOP    (INDX)                  
020553                                  W-SPAR-IDRTLOP                          
020554          MOVE RET-IDKOLLI     TO RESP-IDKOLLI    (INDX)                  
020555                                  W-SPAR-IDKOLLI                          
020557            IF RET-FLFARLIG = JA                                          
020558              MOVE YES           TO RESP-FLFARLIG   (INDX)                
020559            ELSE                                                          
020560              MOVE NEJ           TO RESP-FLFARLIG   (INDX)                
020561            END-IF                                                        
020565          MOVE RET-DARETANK (3:6) TO RESP-TIRETANK   (INDX)               
020566          MOVE RET-ADINLOMR    TO RESP-ADINLOMR   (INDX)                  
020567                                                                          
020568          PERFORM FCA-REDIGERA-BESTATUS                                   
020569                                                                          
020583          ADD 1                TO INDX                                    
020584       END-IF                                                             
020585     END-IF                                                               
020586                                                                          
020587     .                                                                    
020588     EJECT                                                                
020589                                                                          
020590 FCA-REDIGERA-BESTATUS     SECTION.                                       
020591                                                                          
020592     EVALUATE RET-KDKOLSTA                                                
020593                                                                          
020594        WHEN 4                                                            
020595         MOVE 'LOSS'                TO RESP-BESTATUS (INDX)               
020596                                                                          
020597        WHEN 5                                                            
020599           MOVE 'REC '                TO RESP-BESTATUS (INDX)             
020603                                                                          
020604        WHEN 6                                                            
020606           MOVE 'MISS'                TO RESP-BESTATUS (INDX)             
020610                                                                          
020611        WHEN 7                                                            
020613           MOVE 'DEV '                TO RESP-BESTATUS (INDX)             
020618                                                                          
020619     END-EVALUATE                                                         
020620                                                                          
020621                                                                          
020622     IF  WS-COUNT  < 501                                                  
020623      CONTINUE                                                            
020628     ELSE                                                                 
020629        MOVE MORE-LINES TO RESP-IDMSG-ERROR                               
020630     END-IF                                                               
020631                                                                          
020632     MOVE WS-COUNT     TO RESP-KVRADER                                    
020633     .                                                                    
020634     EJECT                                                                
020635                                                                          
020636 FD-LAES-NAESTA-POST      SECTION.                                        
020637                                                                          
020638     EVALUATE TRUE                                                        
020639                                                                          
020640       WHEN IDANSV-SOEK                                                   
020641        PERFORM IMS-GN-RETH-WLRETH01                                      
020642        PERFORM UNTIL SEGMENT-SAKNAS OR SEQG-IDKOLLI = ZERO OR            
020643                      (SEQG-IDRT    NOT = W-SPAR-IDRT   )   OR            
020644                      (SEQG-IDRTLOP NOT = W-SPAR-IDRTLOP)   OR            
020645                      (SEQG-IDKOLLI NOT = W-SPAR-IDKOLLI)                 
020646          PERFORM IMS-GN-RETH-WLRETH01                                    
020647        END-PERFORM                                                       
020648        IF SEGMENT-FINNS                                                  
020649           MOVE SEQG-DAREGDAT      TO W-DAREGDAT                          
020650           MOVE SEQG-TIKLOCK       TO W-TIKLOCK                           
020651                                                                          
020652           PERFORM IMS-GU-RETA-WLRETA01                                   
020653        END-IF                                                            
020654                                                                          
020655       WHEN IDRETSND-SOEK                                                 
020656        PERFORM IMS-GHN-SEQB-WLRETA01                                     
020657        PERFORM UNTIL SEGMENT-SAKNAS OR RET-IDKOLLI = ZERO OR             
020658                      (RET-IDRT    NOT = W-SPAR-IDRT   )   OR             
020659                      (RET-IDRTLOP NOT = W-SPAR-IDRTLOP)   OR             
020660                      (RET-IDKOLLI NOT = W-SPAR-IDKOLLI)                  
020661            PERFORM IMS-GHN-SEQB-WLRETA01                                 
020662        END-PERFORM                                                       
020663                                                                          
020664       WHEN IDDISTR-SOEK                                                  
020665        PERFORM IMS-GN-SEQF-WLRETA01                                      
020666        PERFORM UNTIL SEGMENT-SAKNAS OR RET-IDKOLLI = ZERO OR             
020667                      (RET-IDRT    NOT = W-SPAR-IDRT   ) OR               
020668                      (RET-IDRTLOP NOT = W-SPAR-IDRTLOP) OR               
020669                      (RET-IDKOLLI NOT = W-SPAR-IDKOLLI)                  
020670            PERFORM IMS-GN-SEQF-WLRETA01                                  
020671        END-PERFORM                                                       
020672                                                                          
020673     END-EVALUATE                                                         
020674     .                                                                    
020675     EJECT                                                                
020676 FE-FIXA-NEXT-KEY        SECTION.                                         
020677                                                                          
020678     IF SEGMENT-FINNS                                                     
020682                                                                          
020683        EVALUATE TRUE                                                     
020684                                                                          
020685          WHEN IDANSV-SOEK                                                
020686           MOVE SEQG-IDDC           TO W-MINKEYG1-IDDC-NEXT               
020687           MOVE SEQG-KDRETSTA       TO W-MINKEYG1-KDRETSTA-NEXT           
020688           MOVE SEQG-KDARBTYP       TO W-MINKEYG1-KDARBTYP-NEXT           
020689           MOVE SEQG-IDPERSON       TO W-MINKEYG1-IDPERSON-NEXT           
020690           MOVE SEQG-DARETANK       TO W-MINKEYG1-DARETANK-NEXT           
020691           MOVE SEQG-IDRT           TO W-MINKEYG1-IDRT-NEXT               
020692           MOVE SEQG-IDRTLOP        TO W-MINKEYG1-IDRTLOP-NEXT            
020693           MOVE SEQG-IDKOLLI        TO W-MINKEYG1-IDKOLLI-NEXT            
020694           MOVE SEQG-DAREGDAT       TO W-MINKEYG1-DAREGDAT-NEXT           
020695           MOVE SEQG-TIKLOCK        TO W-MINKEYG1-TIKLOCK-NEXT            
020696           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020697           MOVE '3'                 TO W-MINKEY-IDSOEK                    
020699                                                                          
020700          WHEN IDRETSND-SOEK                                              
020701           MOVE RET-IDDC            TO W-MINKEYB-IDDC-NEXT                
020702           MOVE RET-IDRT            TO W-MINKEYB-IDRT-NEXT                
020703           MOVE RET-IDRTLOP         TO W-MINKEYB-IDRTLOP-NEXT             
020704           MOVE RET-IDKOLLI         TO W-MINKEYB-IDKOLLI-NEXT             
020705           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020706           MOVE '1'                 TO W-MINKEY-IDSOEK                    
020708                                                                          
020709          WHEN IDDISTR-SOEK                                               
020710           MOVE RET-IDDC            TO W-MINKEYF-IDDC-NEXT                
020711           MOVE RET-IDDISTR         TO W-MINKEYF-IDDISTR-NEXT             
020712           MOVE RET-IDKUNDNR        TO W-MINKEYF-IDKUNDNR-NEXT            
020713           MOVE RET-IDRAPPNR        TO W-MINKEYF-IDRAPPNR-NEXT            
020714           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020715           MOVE '2'                 TO W-MINKEY-IDSOEK                    
020717                                                                          
020718        END-EVALUATE                                                      
020719     ELSE                                                                 
020720        EVALUATE TRUE                                                     
020721                                                                          
020722          WHEN IDANSV-SOEK                                                
020723           MOVE REQU-IDDC-KEY       TO W-MINKEYG1-IDDC-NEXT               
020724           MOVE W-SND-MOT           TO W-MINKEYG1-KDRETSTA-NEXT           
020727           MOVE SPACE               TO W-MINKEYG1-IDRT-NEXT               
020728           MOVE ZERO                TO W-MINKEYG1-IDRTLOP-NEXT            
020729                                       W-MINKEYG1-DARETANK-NEXT           
020730                                       W-MINKEYG1-IDKOLLI-NEXT            
020731                                       W-MINKEYG1-DAREGDAT-NEXT           
020732                                       W-MINKEYG1-TIKLOCK-NEXT            
020733           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020734           MOVE '3'                 TO W-MINKEY-IDSOEK                    
020736                                                                          
020737          WHEN IDRETSND-SOEK                                              
020738           MOVE REQU-IDDC-KEY       TO W-MINKEYB-IDDC-NEXT                
020739           MOVE ZERO                TO W-MINKEYB-IDRT-NEXT                
020740                                       W-MINKEYB-IDRTLOP-NEXT             
020741                                       W-MINKEYB-IDKOLLI-NEXT             
020742           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020743           MOVE '1'                 TO W-MINKEY-IDSOEK                    
020745                                                                          
020746          WHEN IDDISTR-SOEK                                               
020747           MOVE REQU-IDDC-KEY       TO W-MINKEYF-IDDC-NEXT                
020748           MOVE REQU-IDDISTR-KEY    TO W-MINKEYF-IDDISTR-NEXT             
020749           MOVE ZERO                TO W-MINKEYF-IDKUNDNR-NEXT            
020750                                       W-MINKEYF-IDRAPPNR-NEXT            
020751           MOVE '4734'              TO W-MINKEY-IDTRANS                   
020752           MOVE '2'                 TO W-MINKEY-IDSOEK                    
020754                                                                          
020755        END-EVALUATE                                                      
020756     END-IF                                                               
020757                                                                          
020758     .                                                                    
020759     EJECT                                                                
020760                                                                          
020761 G-KOLLA-INPUT SECTION.                                                   
020762                                                                          
020763     MOVE JA               TO INDATA-SW                                   
020764                                                                          
020765     PERFORM GA-FORMELL-KONTROLL                                          
020766     IF INDATA-OK                                                         
020767        PERFORM GB-LOGISK-KONTROLL                                        
020768     END-IF                                                               
020777                                                                          
020778     .                                                                    
020779     EJECT                                                                
020780                                                                          
020781 GA-FORMELL-KONTROLL SECTION.                                             
020782                                                                          
020783     IF REQU-INPUT               = ALL '+'                                
020784       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
020789       MOVE NEJ                  TO INDATA-SW                             
020790     ELSE                                                                 
020791       PERFORM GAA-KOLLA-IDANSTNR                                         
020792       PERFORM GAB-KOLLA-KDCMD                                            
020793     END-IF                                                               
020794                                                                          
020795     .                                                                    
020796     EJECT                                                                
020797                                                                          
020798 GAA-KOLLA-IDANSTNR   SECTION.                                            
020799                                                                          
020800                                                                          
020801     IF REQU-IDANSTNR                NOT = ALL '+'                        
020802        IF REQU-IDANSTNR             NUMERIC                              
020804           MOVE REQU-IDANSTNR        TO W-IDANSTNR                        
020806        ELSE                                                              
020809           MOVE '024'                TO RESP-IDMSG-ERROR                  
020810           MOVE 'IDANSTNR'           TO RESP-IDELMT-ERROR                 
020812           MOVE NEJ                  TO INDATA-SW                         
020813        END-IF                                                            
020820     END-IF                                                               
020821                                                                          
020822     .                                                                    
020823     EJECT                                                                
020824 GAB-KOLLA-KDCMD      SECTION.                                            
020825                                                                          
020826     MOVE JA                TO SW-FOERSTA-VALDA                           
020827     MOVE NEJ               TO SW-KOLLI-VALT                              
020828                                                                          
020829     IF REQU-KVRADER NUMERIC AND REQU-KVRADER > 0                         
020830        MOVE REQU-KVRADER                  TO WS-INDX-REC                 
020831        MOVE NEJ                           TO WS-REC-LIMIT                
020832        MOVE +1                TO INDX                                    
020833                                                                          
020834     PERFORM UNTIL INDX                 >  MAX-INDX  OR REC-LIMIT         
020835        IF REQU-KDCMD(INDX)              NOT = ALL '+' AND SPACE          
020843                                                                          
020845           IF REQU-KDCMD(INDX)          = W-BINNED OR                     
020846                                          W-SELECTCASE                    
020849                                                                          
020851             IF FOERSTA-VALDA-KOLLI                                       
020852                MOVE NEJ               TO SW-FOERSTA-VALDA                
020853                IF REQU-KDCMD(INDX)     =  W-SELECTCASE                   
020855                   MOVE JA             TO SW-KOLLI-VALT                   
020856                END-IF                                                    
020858                IF REQU-KDCMD(INDX)     =  W-BINNED                       
020859                   MOVE JA             TO SW-KOLLI-INL                    
020860                END-IF                                                    
020864             ELSE                                                         
020865                IF REQU-KDCMD(INDX)     =  W-SELECTCASE OR                
020867                   KOLLI-VALT                                             
020871                   MOVE NEJ            TO INDATA-SW                       
020874                   MOVE '023'          TO RESP-IDMSG-ERROR                
020875                   MOVE '023' TO  RESP-IDMSG-ERROR-LINE(INDX)             
020876                   MOVE 'CMD'          TO RESP-IDELMT-ERROR               
020877                END-IF                                                    
020892                IF REQU-KDCMD(INDX)     =  W-BINNED                       
020893                   IF KOLLI-INL                                           
020896                     MOVE '023'          TO RESP-IDMSG-ERROR              
020899                     MOVE '023' TO  RESP-IDMSG-ERROR-LINE(INDX)           
020900                     MOVE 'CMD'          TO RESP-IDELMT-ERROR             
020901                     MOVE NEJ            TO INDATA-SW                     
020902                   END-IF                                                 
020903                END-IF                                                    
020904             END-IF                                                       
020905           ELSE                                                           
020907             MOVE NEJ                  TO INDATA-SW                       
020908             MOVE '023'                TO RESP-IDMSG-ERROR                
020909             MOVE '023' TO       RESP-IDMSG-ERROR-LINE(INDX)              
020910             MOVE 'CMD'                TO RESP-IDELMT-ERROR               
020913           END-IF                                                         
020914        END-IF                                                            
020916                                                                          
020917          IF INDX = WS-INDX-REC                                           
020918             MOVE JA TO WS-REC-LIMIT                                      
020919          ELSE                                                            
020920             ADD +1                       TO INDX                         
020921          END-IF                                                          
020922                                                                          
020923     END-PERFORM                                                          
020925     ELSE                                                                 
020926        MOVE NEJ           TO INDATA-SW                                   
020927        IF REQU-KVRADER = 0                                               
020928           MOVE 'KVRADER' TO RESP-IDELMT-ERROR                            
020929           MOVE '126'     TO RESP-IDMSG-ERROR                             
020930        ELSE                                                              
020931           MOVE 'KVRADER' TO RESP-IDELMT-ERROR                            
020932           MOVE '024'     TO RESP-IDMSG-ERROR                             
020933        END-IF                                                            
020934     END-IF                                                               
020935                                                                          
020936     .                                                                    
020937     EJECT                                                                
020938                                                                          
020939 GB-LOGISK-KONTROLL SECTION.                                              
020940                                                                          
020941     MOVE +1                 TO INDX                                      
020942     MOVE NEJ                TO WS-REC-LIMIT                              
020943                                                                          
020944     PERFORM UNTIL INDX      >  MAX-INDX OR REC-LIMIT                     
020951                                                                          
020953        IF REQU-KDCMD(INDX)   =  W-BINNED OR                              
020954                                 W-SELECTCASE                             
020959           PERFORM GBA-KOLLA-VALT-KOLLI                                   
020960        END-IF                                                            
020961                                                                          
020962       IF INDX = WS-INDX-REC                                              
020963          MOVE JA TO WS-REC-LIMIT                                         
020964       ELSE                                                               
020965          ADD +1                       TO INDX                            
020966       END-IF                                                             
020968     END-PERFORM                                                          
020969                                                                          
020970     .                                                                    
020971     EJECT                                                                
020972 GBA-KOLLA-VALT-KOLLI             SECTION.                                
020973                                                                          
020974     MOVE REQU-IDRT(INDX)           TO W-IDRT-BSEQ-MIN                    
020975                                       W-IDRT-BSEQ-MAX                    
020976     MOVE REQU-IDRTLOP(INDX)        TO W-IDRTLOP-BSEQ-MIN                 
020977                                       W-IDRTLOP-BSEQ-MAX                 
020978     INSPECT REQU-IDKOLLI (INDX) REPLACING LEADING SPACE BY ZERO          
020979     MOVE REQU-IDKOLLI(INDX)        TO W-IDKOLLI-BSEQ-MIN                 
020980                                       W-IDKOLLI-BSEQ-MAX                 
020981     PERFORM IMS-GHU-SEQB-WLRETA01                                        
020982     IF SEGMENT-FINNS                                                     
020983                                                                          
020985        IF REQU-KDCMD (INDX)     = W-BINNED                               
020986          IF RET-KDRETSTA            =  W-SND-MOT AND                     
020987             RET-KDKOLSTA            =  W-KLI-MOT                         
020988                PERFORM GBAA-KOLLA-VALT-RT                                
020989          ELSE                                                            
020991              MOVE NEJ                TO INDATA-SW                        
020992              MOVE '023'              TO RESP-IDMSG-ERROR                 
020993              MOVE '023' TO    RESP-IDMSG-ERROR-LINE(INDX)                
020994              MOVE 'CMD'              TO RESP-IDELMT-ERROR                
020996          END-IF                                                          
020997        END-IF                                                            
020998        IF REQU-KDCMD (INDX)     = W-SELECTCASE                           
020999          IF RET-KDKOLSTA            =  W-KLI-AVV OR                      
021000                                        W-KLI-SAK OR                      
021001                                        W-KLI-VALT OR                     
021002                                        W-KLI-LOSS OR                     
021003                                        W-KLI-BEH                         
021005              MOVE NEJ                TO INDATA-SW                        
021006              MOVE '023'              TO RESP-IDMSG-ERROR                 
021007              MOVE '023' TO    RESP-IDMSG-ERROR-LINE(INDX)                
021008              MOVE 'CMD'              TO RESP-IDELMT-ERROR                
021009          END-IF                                                          
021010        END-IF                                                            
021011                                                                          
021014*HÄR SKALL EN DATUM KONTROLL IN OXÅ + EV ANDRA KONTROLLER                 
021015*EXEMPELVIS KONTROLL ATT RT'S ÖVRIGA KOLLIN ÄR INLAGDA OSV                
021022     ELSE                                                                 
021024        MOVE NEJ                       TO INDATA-SW                       
021025        MOVE '023'                     TO RESP-IDMSG-ERROR                
021026        MOVE '023' TO                RESP-IDMSG-ERROR-LINE(INDX)          
021027        MOVE 'CMD'                     TO RESP-IDELMT-ERROR               
021030     END-IF                                                               
021031     .                                                                    
021032     EJECT                                                                
021033 GBAA-KOLLA-VALT-RT                SECTION.                               
021034                                                                          
021035     PERFORM UNTIL SEGMENT-SAKNAS                                         
021036                                                                          
021037** AKTUELLT RETURTILLSTÅND FÅR BARA FINNAS I ETT KOLLI                    
021038                                                                          
021039        MOVE LOW-VALUE             TO W-WDA3F1KY-MIN-X                    
021040        MOVE HIGH-VALUE            TO W-WDA3F1KY-MAX-X                    
021041                                                                          
021042        MOVE RET-IDDC              TO W-IDDC-F1-MIN                       
021043                                      W-IDDC-F1-MAX                       
021044        MOVE RET-IDDISTR           TO W-IDDISTR-F1-MIN                    
021045                                      W-IDDISTR-F1-MAX                    
021046        MOVE RET-IDKUNDNR          TO W-IDKUNDNR-F1-MIN                   
021047                                      W-IDKUNDNR-F1-MAX                   
021048        MOVE RET-IDRAPPNR          TO W-IDRAPPNR-F1-MIN                   
021049                                      W-IDRAPPNR-F1-MAX                   
021050        PERFORM IMS-GU-WLRETG01                                           
021051        IF SEGMENT-FINNS                                                  
021052           PERFORM IMS-GN-WLRETG01                                        
021053           IF SEGMENT-FINNS                                               
021055               MOVE NEJ                TO INDATA-SW                       
021056               MOVE '023'              TO RESP-IDMSG-ERROR                
021057               MOVE '023' TO         RESP-IDMSG-ERROR-LINE(INDX)          
021058               MOVE 'CMD'              TO RESP-IDELMT-ERROR               
021061           END-IF                                                         
021062        END-IF                                                            
021063                                                                          
021064        PERFORM IMS-GHN-SEQB-WLRETA01                                     
021065     END-PERFORM                                                          
021066                                                                          
021067     .                                                                    
021068     EJECT                                                                
021069 H-UPPDATERA SECTION.                                                     
021070                                                                          
021071     MOVE NEJ                    TO W-UPDATE-SW                           
021072     MOVE +1                     TO 4792-INDX                             
021073                                    4797-INDX                             
021074     ACCEPT DAGENS-DATUM         FROM DATE                                
021075                                                                          
021076     MOVE NEJ                TO WS-REC-LIMIT                              
021077     MOVE +1                 TO INDX                                      
021078     PERFORM UNTIL INDX      >  MAX-INDX OR REC-LIMIT                     
021079                                                                          
021088        IF REQU-KDCMD(INDX)   = W-BINNED OR                               
021089                                W-SELECTCASE                              
021092           PERFORM HA-UPPDATERA-KOLLI                                     
021093        END-IF                                                            
021094                                                                          
021095       IF INDX = WS-INDX-REC                                              
021096          MOVE JA TO WS-REC-LIMIT                                         
021097       ELSE                                                               
021098          ADD +1                       TO INDX                            
021099       END-IF                                                             
021100     END-PERFORM                                                          
021101                                                                          
021102     IF 4797-INDX              >  +1                                      
021103        IF 4792-INDX              >  +1                                   
021104           PERFORM S02A-STARTA-R31-RAPPORTERING                           
021105        END-IF                                                            
021106        PERFORM S03-STARTA-R32-RAPPORTERING                               
021107     END-IF                                                               
021110                                                                          
021111     IF  W-UPDATE-OK                                                      
021112        MOVE INF-UPDATE-DONE     TO RESP-IDMSG-INFO                       
021113     ELSE                                                                 
021114        MOVE '004'               TO RESP-IDMSG-INFO                       
021115     END-IF                                                               
021121     .                                                                    
021122     EJECT                                                                
021123                                                                          
021124 HA-UPPDATERA-KOLLI        SECTION.                                       
021125                                                                          
021126     MOVE REQU-IDRT(INDX)          TO W-IDRT-BSEQ-MIN                     
021127                                      W-IDRT-BSEQ-MAX                     
021128     MOVE REQU-IDRTLOP(INDX)       TO W-IDRTLOP-BSEQ-MIN                  
021129                                      W-IDRTLOP-BSEQ-MAX                  
021130     INSPECT REQU-IDKOLLI (INDX) REPLACING LEADING SPACE BY ZERO          
021131     MOVE REQU-IDKOLLI(INDX)       TO W-IDKOLLI-BSEQ-MIN                  
021132                                      W-IDKOLLI-BSEQ-MAX                  
021133     PERFORM IMS-GHU-SEQB-WLRETA01                                        
021134                                                                          
021135     IF SEGMENT-FINNS                                                     
021136        PERFORM UNTIL SEGMENT-SAKNAS                                      
021137                                                                          
021138           IF RET-DARETANK = ZERO                                         
021139              PERFORM S02-FYLL-R31-MID                                    
021140           END-IF                                                         
021141                                                                          
021143           IF REQU-KDCMD (INDX)    =  W-BINNED                            
021146              PERFORM HBAA-UPPDATERA-LEV-ANM-REG                          
021147                                                                          
021148              MOVE W-SND-INL       TO RET-KDRETSTA                        
021149              MOVE W-KLI-BEH       TO RET-KDKOLSTA                        
021150              MOVE DAGENS-DATUM    TO RET-TIKLAR                          
021151              MOVE SPACE           TO RESP-KDCMD (INDX)                   
021152           ELSE                                                           
021153              MOVE RET-IDDISTR             TO W-IDDISTR-ANM               
021154              MOVE RET-IDKUNDNR            TO W-IDKUNDNR-ANM              
021155              MOVE RET-IDRAPPNR            TO W-IDRAPPNR-ANM              
021156                                                                          
021157              PERFORM IMS-GHU-WLKREE01                                    
021158              IF ANM-KDLEVANM = W-ANM-MOT                                 
021159                MOVE W-ANM-PAAB            TO ANM-KDLEVANM                
021160                PERFORM IMS-REPL-WLKREE01                                 
021169              END-IF                                                      
021170                                                                          
021171**- TEST FÖR ATT EJ SKRIVA ÖVER REDAN KLAR LEV.ANM . FÖR ATT              
021172**- KOMMA MED I RENSNINGSPGM W4189000,SÅ MÅSTE RET-KDRETSTA = 6           
021173**- W-KLI-BEH    PIC S9(1)  VALUE +9 COMP-3.                              
021174**- W-SND-INL    PIC X(1)   VALUE '6'.                                    
021175                                                                          
021176              IF RET-KDRETSTA = W-SND-INL                                 
021177                CONTINUE                                                  
021178              ELSE                                                        
021179                MOVE W-SND-PAAB      TO RET-KDRETSTA                      
021180              END-IF                                                      
021181              IF RET-KDKOLSTA = W-KLI-BEH                                 
021182                CONTINUE                                                  
021183              ELSE                                                        
021184                MOVE W-KLI-VALT      TO RET-KDKOLSTA                      
021185              END-IF                                                      
021186           END-IF                                                         
021187                                                                          
021188           PERFORM IMS-REPL-SEQB-WLRETA01                                 
021189           PERFORM IMS-GHN-SEQB-WLRETA01                                  
021190        END-PERFORM                                                       
021191                                                                          
021195        MOVE JA           TO  W-UPDATE-SW                                 
021196     ELSE                                                                 
021197        CALL FELLOG                                                       
021198        MOVE NEJ          TO  W-UPDATE-SW                                 
021199     END-IF                                                               
021200     .                                                                    
021201     EJECT                                                                
021202                                                                          
021203 HBAA-UPPDATERA-LEV-ANM-REG  SECTION.                                     
021204                                                                          
021205     MOVE RET-IDDISTR             TO W-IDDISTR-ANM                        
021206     MOVE RET-IDKUNDNR            TO W-IDKUNDNR-ANM                       
021207     MOVE RET-IDRAPPNR            TO W-IDRAPPNR-ANM                       
021208                                                                          
021209     PERFORM IMS-GHU-WLKREE01                                             
021210     MOVE ZERO                    TO ANM-KVRADER-OBEH                     
021211     IF ANM-KDLEVANM = W-ANM-MOT                                          
021212       MOVE W-ANM-PAAB              TO ANM-KDLEVANM                       
021213     END-IF                                                               
021214     PERFORM IMS-REPL-WLKREE01                                            
021215                                                                          
021216     PERFORM S04-LAES-WLKREE11                                            
021217     PERFORM UNTIL SEGMENT-SAKNAS                                         
021218        COMPUTE W-KVRADER-KVAR    =  LEV-KVLEVANM-BEKR -                  
021219                                     LEV-KVRETINL -                       
021220                                     LEV-KVAVV-KVANT -                    
021221                                     LEV-KVRETINL-SKR -                   
021222                                     LEV-KVAVV-KVAL                       
021223                                                                          
021224        COMPUTE LEV-KVRETINL      = LEV-KVRETINL +                        
021225                                     W-KVRADER-KVAR                       
021226                                                                          
021227        IF REQU-IDANSTNR NUMERIC                                          
021228          MOVE REQU-IDANSTNR      TO LEV-IDANSTNR-RET                     
021230        END-IF                                                            
021231                                                                          
021232        MOVE ZERO                 TO LEV-IDILIST                          
021233                                     LEV-TIUTSKR                          
021234                                     LEV-KVANTAL-ILI                      
021235                                     LEV-TIUPPDAT-ILI                     
021236                                                                          
021237        ACCEPT LEV-TIINLINL FROM DATE                                     
021238                                                                          
021239        MOVE RET-IDDISTR    TO MOD4797-MID-IDDISTR(4797-INDX)             
021240        MOVE RET-IDKUNDNR   TO MOD4797-MID-IDKUNDNR(4797-INDX)            
021250        MOVE RET-IDRAPPNR   TO MOD4797-MID-IDRAPPNR(4797-INDX)            
021260        MOVE LEV-IDARTNR    TO MOD4797-MID-IDARTNR(4797-INDX)             
021261        MOVE LEV-IDRADNR    TO MOD4797-MID-IDRADNR(4797-INDX)             
021270        COMPUTE MOD4797-MID-KVRETINL(4797-INDX) =                         
021271                LEV-KVRETINL +                                            
021272                LEV-KVRETINL-SKR                                          
021273        COMPUTE MOD4797-MID-KVAVV-KVANT(4797-INDX) =                      
021274                LEV-KVAVV-KVAL + LEV-KVAVV-KVANT                          
021275                                                                          
021277        MOVE ZERO           TO MOD4797-MID-KVRETINL-TRP(4797-INDX)        
021278                               MOD4797-MID-KVRETINL-SKR(4797-INDX)        
021280                                                                          
021281        PERFORM IMS-REPL-WLKREE11                                         
021282                                                                          
021283        PERFORM S04-LAES-WLKREE11                                         
021284                                                                          
021285        ADD +1                 TO 4797-INDX                               
021286                                                                          
021287        IF 4797-INDX           >  4797-MAX-INDX                           
021288           IF 4792-INDX              >  +1                                
021289              PERFORM S02A-STARTA-R31-RAPPORTERING                        
021290              MOVE +1             TO 4792-INDX                            
021291           END-IF                                                         
021292           PERFORM S03-STARTA-R32-RAPPORTERING                            
021293           MOVE +1             TO 4797-INDX                               
021294        END-IF                                                            
021295                                                                          
021296     END-PERFORM                                                          
021297     .                                                                    
021298     EJECT                                                                
021299                                                                          
021331 S02-FYLL-R31-MID                SECTION.                                 
021332                                                                          
021333     MOVE RET-IDDC             TO MOD4792-MID-IDDC                        
021334     MOVE RET-DAREGDAT (3:6)   TO MOD4792-MID-TIREGDAT(4792-INDX)         
021335     MOVE RET-TIKLOCK          TO MOD4792-MID-TIKLOCK (4792-INDX)         
021336     ADD +1                    TO 4792-INDX                               
021337                                                                          
021338     IF 4792-INDX              >  4792-MAX-INDX                           
021339        PERFORM S02A-STARTA-R31-RAPPORTERING                              
021340        MOVE +1                TO 4792-INDX                               
021341     END-IF                                                               
021342     .                                                                    
021343     EJECT                                                                
021344                                                                          
021345 S02A-STARTA-R31-RAPPORTERING    SECTION.                                 
021346                                                                          
021347     ACCEPT DAGENS-DATUM       FROM DATE                                  
021348     ACCEPT DAGENS-TID         FROM TIME                                  
021349                                                                          
021350     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
021352     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
021353     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
021354     MOVE SPACE                TO MSG-KOM-KDTRANS                         
021355     MOVE 'W4I79201'           TO MSG-KOM-IDCPYTXT                        
021356     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
021357     MOVE 'W4073100'           TO MSG-KOM-IDSNDJOB                        
021358     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
021359     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
021360     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
021361                                                                          
021362     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
021363                                  LENGTH OF MOD4792-MID-W4I79201          
021364                                                                          
021365     MOVE 'W4T792X '           TO P-TO-P-MSG-KDTRANS                      
021366     MOVE 'L149'               TO P-TO-P-MSG-IDTRANS                      
021367     MOVE '2'                  TO P-TO-P-MSG-KDMFSFOR                     
021368                                                                          
021369     COMPUTE MOD4792-MID-KVPOST  = 4792-INDX - 1                          
021370                                                                          
021371     MOVE MOD4792-MID-W4I79201 TO P-TO-P-MSG-INDATA                       
021372                                                                          
021373     CALL W006KOM USING MSG-PCB                                           
021374                        DISP-PCB                                          
021375                        KOM-KOMA-PCB                                      
021376                        MSG-KOM-WMSGKOM                                   
021377                        P-TO-P-MSG-IO-AREA-SNUF                           
021378                                                                          
021379     .                                                                    
021380     EJECT                                                                
021381 S03-STARTA-R32-RAPPORTERING     SECTION.                                 
021382                                                                          
021383     ACCEPT DAGENS-DATUM       FROM DATE                                  
021384     ACCEPT DAGENS-TID         FROM TIME                                  
021385                                                                          
021386     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
021388     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
021389     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
021390     MOVE SPACE                TO MSG-KOM-KDTRANS                         
021391     MOVE 'W4I79701'           TO MSG-KOM-IDCPYTXT                        
021392     MOVE 'INLEVRET'           TO MSG-KOM-IDSNDNOD                        
021393     MOVE 'W4073400'           TO MSG-KOM-IDSNDJOB                        
021394     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
021395     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
021396     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
021397                                                                          
021398     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
021399                                  LENGTH OF MOD4797-MID-W4I79701          
021400                                                                          
021401     MOVE 'W4T797X '           TO P-TO-P-MSG-KDTRANS                      
021402     MOVE 'L149'               TO P-TO-P-MSG-IDTRANS                      
021403     MOVE '2'                  TO P-TO-P-MSG-KDMFSFOR                     
021404                                                                          
021405     COMPUTE MOD4797-MID-KVPOST  = 4797-INDX - 1                          
021406                                                                          
021407     MOVE MOD4797-MID-W4I79701 TO P-TO-P-MSG-INDATA                       
021408                                                                          
021409     CALL W006KOM USING MSG-PCB                                           
021410                        DISP-PCB                                          
021411                        KOM-KOMA-PCB                                      
021412                        MSG-KOM-WMSGKOM                                   
021413                        P-TO-P-MSG-IO-AREA-SNUF                           
021414                                                                          
021415     .                                                                    
021416     EJECT                                                                
021417 S04-LAES-WLKREE11        SECTION.                                        
021418                                                                          
021419     MOVE NEJ                    TO OKOD-FL-RETILL                        
021420                                    OKOD-FL-INTERNUPPACKNING              
021421     PERFORM IMS-GHNP-WLKREE11                                            
021422     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
021423                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
021424        IF LEV-KDKREBEH(1:1) = 'Y' OR                                     
021425           LEV-KDKREBEH(1:1) = 'J' OR                                     
021426           LEV-KDKREBEH(1:1) = 'C'                                        
021427          IF LEV-IDARTNR NOT = 100                                        
021428            MOVE LEV-KDANMORS TO OKOD-KDANMORS                            
021429*--ANROPA KONTROLL AV ORSAKSKODER                                         
021430            CALL W418OKOD USING OKOD-W418OKOD                             
021431          END-IF                                                          
021432        END-IF                                                            
021433        IF OKOD-FL-RETILL = 'J' OR                                        
021434           OKOD-FL-INTERNUPPACKNING = 'J'                                 
021435           CONTINUE                                                       
021436        ELSE                                                              
021437          PERFORM IMS-GHNP-WLKREE11                                       
021438        END-IF                                                            
021439     END-PERFORM                                                          
021440     .                                                                    
021441     EJECT                                                                
021442 S05-BERAKNA-DATUM        SECTION.                                        
021443                                                                          
021444     PERFORM IMS-GU-WL410711                                              
021445                                                                          
021446     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
021447                                                                          
021448     CALL WDATKONV USING DAT-KDDATFORM                                    
021449                         DAT-I-TIDATUM                                    
021450                         DAT-O-TIDATUM                                    
021451                         DAT-KDSVAR                                       
021452     IF DAT-KDSVAR-FEL                                                    
021453        MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                              
021454        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
021455     END-IF                                                               
021456                                                                          
021457     MOVE DAT-TIAADDD     TO W-TIAADDD-IDAG                               
021458                                                                          
021459     IF 4108-KVDAGAR-KLIARB >= W-TIDDD-IDAG                               
021460        IF W-TIAA-IDAG       = 00                                         
021461          MOVE 99            TO W-TIAA                                    
021462        ELSE                                                              
021463          COMPUTE W-TIAA     = W-TIAA-IDAG  - 1                           
021464        END-IF                                                            
021465        COMPUTE W-TIDDD         =  365                 -                  
021466                                   4108-KVDAGAR-KLIARB +                  
021467                                   W-TIDDD-IDAG                           
021468        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
021469        MOVE 'AADDD'            TO DAT-KDDATFORM                          
021470                                                                          
021471        CALL WDATKONV USING DAT-KDDATFORM                                 
021472                            DAT-I-TIDATUM                                 
021473                            DAT-O-TIDATUM                                 
021474                            DAT-KDSVAR                                    
021475        IF DAT-KDSVAR-FEL                                                 
021476           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
021477           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
021478        END-IF                                                            
021479                                                                          
021480        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-KLIARB                      
021481        MOVE DAT-TISEKEL        TO W-TISEKEL-KLIARB                       
021482     ELSE                                                                 
021483        MOVE W-TIAA-IDAG        TO W-TIAA                                 
021484        COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                         
021485                                   4108-KVDAGAR-KLIARB                    
021486        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
021487        MOVE 'AADDD'            TO DAT-KDDATFORM                          
021488                                                                          
021489        CALL WDATKONV USING DAT-KDDATFORM                                 
021490                            DAT-I-TIDATUM                                 
021491                            DAT-O-TIDATUM                                 
021492                            DAT-KDSVAR                                    
021493        IF DAT-KDSVAR-FEL                                                 
021494           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
021495           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
021496        END-IF                                                            
021497                                                                          
021498        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-KLIARB                      
021499        MOVE DAT-TISEKEL        TO W-TISEKEL-KLIARB                       
021500     END-IF                                                               
021501                                                                          
021502     IF 4108-KVDAGAR-KLIAVV >= W-TIDDD-IDAG                               
021503        IF W-TIAA-IDAG          = 00                                      
021504          MOVE 99               TO W-TIAA                                 
021505        ELSE                                                              
021506          COMPUTE W-TIAA        = W-TIAA-IDAG  - 1                        
021507        END-IF                                                            
021508        COMPUTE W-TIDDD         =  365                 -                  
021509                                   4108-KVDAGAR-KLIAVV +                  
021510                                   W-TIDDD-IDAG                           
021511        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
021512        MOVE 'AADDD'            TO DAT-KDDATFORM                          
021513                                                                          
021514        CALL WDATKONV USING DAT-KDDATFORM                                 
021515                            DAT-I-TIDATUM                                 
021516                            DAT-O-TIDATUM                                 
021517                            DAT-KDSVAR                                    
021518        IF DAT-KDSVAR-FEL                                                 
021519           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
021520           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
021521        END-IF                                                            
021522                                                                          
021523        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-KLIAVV                      
021524        MOVE DAT-TISEKEL        TO W-TISEKEL-KLIAVV                       
021525     ELSE                                                                 
021526        MOVE W-TIAA-IDAG        TO W-TIAA                                 
021527        COMPUTE W-TIDDD         =  W-TIDDD-IDAG -                         
021528                                   4108-KVDAGAR-KLIARB                    
021529        MOVE W-TIAADDD          TO DAT-I-TIDATUM                          
021530        MOVE 'AADDD'            TO DAT-KDDATFORM                          
021531                                                                          
021532        CALL WDATKONV USING DAT-KDDATFORM                                 
021533                            DAT-I-TIDATUM                                 
021534                            DAT-O-TIDATUM                                 
021535                            DAT-KDSVAR                                    
021536        IF DAT-KDSVAR-FEL                                                 
021537           MOVE 'FEL FRÅN DATKONV'   TO FELTEXT                           
021538           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
021539        END-IF                                                            
021540                                                                          
021541        MOVE DAT-TIAAMMDD       TO W-TIAAMMDD-KLIAVV                      
021542        MOVE DAT-TISEKEL        TO W-TISEKEL-KLIAVV                       
021543     END-IF                                                               
021544     .                                                                    
021545     EJECT                                                                
021546*    --- DISPATCHER SECTIONS                                              
021547 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
021548                                                                          
021549     MOVE 'GETARG'               TO SUB-KDFUNC                            
021550     MOVE 'CARPARTS.LDC.SHOWCASEQUEUE'      TO SUB-ADDISPABS              
021551     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
021552                                                                          
021553     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
021554                                                                          
021555     IF SUB-KDRC > 0                                                      
021556       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021557       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
021558       DELIMITED BY SIZE INTO ERROR-TEXT                                  
021559       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
021560     END-IF                                                               
021570     .                                                                    
021600     SKIP3                                                                
021700 S02-RETURN-RESPONSE SECTION.                                             
021800                                                                          
021900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
022000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
022100                                                                          
022200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
022300                                                                          
022400     IF SUB-KDRC > 0                                                      
022500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
022600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
022700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
022800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022900     END-IF                                                               
023000     .                                                                    
023100     EJECT                                                                
023110* --- IMS SEKTIONER ---                                                   
023120     SKIP3                                                                
023200 IMS-GU-WL410711            SECTION.                                      
023300                                                                          
023400     STRING 'WL410701(WDGXKEY  =' W-WDGX4107-X ')'                        
023500          DELIMITED BY SIZE INTO SSA1                                     
023600     STRING 'WL410711(KDSEGKEY =' W-KDSEGKEY-X ')'                        
023700          DELIMITED BY SIZE INTO SSA2                                     
023800     MOVE '  '           TO GODK-STATUSKODER                              
023900     CALL CBLTDLI USING GU 4107-PCB DLI-IO-AREA2 SSA1 SSA2                
024000     MOVE 4107-STATUS-CODE TO STATUS-WS                                   
024100     PERFORM IMS-STATUSKONTROLL                                           
024200     .                                                                    
024300 IMS-GHU-SEQB-WLRETA01       SECTION.                                     
024400                                                                          
024500     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
024600                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
024700          DELIMITED BY SIZE INTO SSA1                                     
024800     MOVE '  GE'           TO GODK-STATUSKODER                            
024900     CALL CBLTDLI USING GHU RETA1-PCB DLI-IO-AREA1 SSA1                   
025000     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
025100     PERFORM IMS-STATUSKONTROLL                                           
025200     .                                                                    
025300                                                                          
025400 IMS-GHN-SEQB-WLRETA01 SECTION.                                           
025500                                                                          
025600     STRING 'WLRETA01(WDA3BSEQ>=' W-WDA3BSEQ-MIN-X                        
025700                    '&WDA3BSEQ<=' W-WDA3BSEQ-MAX-X ')'                    
025800          DELIMITED BY SIZE INTO SSA1                                     
025900     MOVE '  GE' TO GODK-STATUSKODER                                      
026000     CALL CBLTDLI USING GHN RETA1-PCB DLI-IO-AREA1 SSA1                   
026100     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
026200     PERFORM IMS-STATUSKONTROLL                                           
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600 IMS-REPL-SEQB-WLRETA01      SECTION.                                     
026700                                                                          
026800     MOVE '    '           TO GODK-STATUSKODER                            
026900     CALL CBLTDLI USING REPL RETA1-PCB DLI-IO-AREA1                       
027000     MOVE RETA1-STATUS-CODE TO STATUS-WS                                  
027100     PERFORM IMS-STATUSKONTROLL                                           
027200     .                                                                    
027300     EJECT                                                                
027400                                                                          
027500 IMS-GU-SEQF-WLRETA01       SECTION.                                      
027600                                                                          
027700     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
027800                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
027900          DELIMITED BY SIZE INTO SSA1                                     
028000     MOVE '  GE'           TO GODK-STATUSKODER                            
028100     CALL CBLTDLI USING GU RETA2-PCB DLI-IO-AREA1 SSA1                    
028200     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
028300     PERFORM IMS-STATUSKONTROLL                                           
028400     .                                                                    
028500                                                                          
028600 IMS-GN-SEQF-WLRETA01 SECTION.                                            
028700                                                                          
028800     STRING 'WLRETA01(WDA3FSEQ>=' W-WDA3FSEQ-MIN-X                        
028900                    '&WDA3FSEQ<=' W-WDA3FSEQ-MAX-X ')'                    
029000          DELIMITED BY SIZE INTO SSA1                                     
029100     MOVE '  GE' TO GODK-STATUSKODER                                      
029200     CALL CBLTDLI USING GN RETA2-PCB DLI-IO-AREA1 SSA1                    
029300     MOVE RETA2-STATUS-CODE TO STATUS-WS                                  
029400     PERFORM IMS-STATUSKONTROLL                                           
029500     .                                                                    
029600     EJECT                                                                
029700                                                                          
029800 IMS-GU-RETA-WLRETA01       SECTION.                                      
029900                                                                          
030000     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
030100          DELIMITED BY SIZE INTO SSA1                                     
030200     MOVE '  GE'           TO GODK-STATUSKODER                            
030300     CALL CBLTDLI USING GU RETA3-PCB DLI-IO-AREA1 SSA1                    
030400     MOVE RETA3-STATUS-CODE TO STATUS-WS                                  
030500     PERFORM IMS-STATUSKONTROLL                                           
030600     .                                                                    
030700                                                                          
030800 IMS-GU-RETH-WLRETH01    SECTION.                                         
030900                                                                          
031000     STRING 'WLRETH01(WDA3G1KY>=' W-WDA3G1KY-MIN-X                        
031100                    '&WDA3G1KY<=' W-WDA3G1KY-MAX-X                        
031200                    '&KDARBTYP =' W-KDARBTYP-X                            
031300                    '&IDPERSON =' W-IDPERSON-X ')'                        
031400          DELIMITED BY SIZE INTO SSA1                                     
031500     MOVE '  GE' TO GODK-STATUSKODER                                      
031600     CALL CBLTDLI USING GU RETH-PCB DLI-IO-AREA2 SSA1                     
031700     MOVE RETH-STATUS-CODE TO STATUS-WS                                   
031800     PERFORM IMS-STATUSKONTROLL                                           
031900     .                                                                    
032000     SKIP2                                                                
032100 IMS-GN-RETH-WLRETH01    SECTION.                                         
032200                                                                          
032300     STRING 'WLRETH01(WDA3G1KY>=' W-WDA3G1KY-MIN-X                        
032400                    '&WDA3G1KY<=' W-WDA3G1KY-MAX-X                        
032500                    '&KDARBTYP =' W-KDARBTYP-X                            
032600                    '&IDPERSON =' W-IDPERSON-X ')'                        
032700          DELIMITED BY SIZE INTO SSA1                                     
032800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
032900     CALL CBLTDLI USING GN RETH-PCB DLI-IO-AREA2 SSA1                     
033000     MOVE RETH-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300     SKIP2                                                                
033400 IMS-GU-WLRETG01    SECTION.                                              
033500                                                                          
033600     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
033700                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
033800          DELIMITED BY SIZE INTO SSA1                                     
033900     MOVE '  ' TO GODK-STATUSKODER                                        
034000     CALL CBLTDLI USING GU RETG-PCB DLI-IO-AREA1 SSA1                     
034100     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
034200     PERFORM IMS-STATUSKONTROLL                                           
034300     .                                                                    
034400     SKIP2                                                                
034500                                                                          
034600 IMS-GN-WLRETG01    SECTION.                                              
034700                                                                          
034800     STRING 'WLRETG01(WDA3F1KY>=' W-WDA3F1KY-MIN-X                        
034900                    '&WDA3F1KY<=' W-WDA3F1KY-MAX-X ')'                    
035000          DELIMITED BY SIZE INTO SSA1                                     
035100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
035200     CALL CBLTDLI USING GN RETG-PCB DLI-IO-AREA1 SSA1                     
035300     MOVE RETG-STATUS-CODE TO STATUS-WS                                   
035400     PERFORM IMS-STATUSKONTROLL                                           
035500     .                                                                    
035600     SKIP2                                                                
035700 IMS-GHU-WLKREE01    SECTION.                                             
035800                                                                          
035900     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
036000          DELIMITED BY SIZE INTO SSA1                                     
036100     MOVE '  ' TO GODK-STATUSKODER                                        
036200     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA2 SSA1                    
036300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
036400     PERFORM IMS-STATUSKONTROLL                                           
036500     .                                                                    
036600                                                                          
036700 IMS-REPL-WLKREE01      SECTION.                                          
036800                                                                          
036900     MOVE '    '           TO GODK-STATUSKODER                            
037000     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA2                        
037100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
037200     PERFORM IMS-STATUSKONTROLL                                           
037300     .                                                                    
037400     EJECT                                                                
037500                                                                          
037600 IMS-GHNP-WLKREE11    SECTION.                                            
037700                                                                          
037800     MOVE 'WLKREE11 '  TO SSA1                                            
037900     MOVE '  GE' TO GODK-STATUSKODER                                      
038000     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA2 SSA1                   
038100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400                                                                          
038500 IMS-REPL-WLKREE11      SECTION.                                          
038600                                                                          
038700     MOVE '    '           TO GODK-STATUSKODER                            
038800     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA2                        
038900     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
039000     PERFORM IMS-STATUSKONTROLL                                           
039100     .                                                                    
039200     EJECT                                                                
039300 IMS-STATUSKONTROLL SECTION.                                              
039400                                                                          
039500     SET STATUS-IX TO 1                                                   
039600     SEARCH GODK-STATUS                                                   
039700       AT END                                                             
039800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039900         DELIMITED BY SIZE INTO FELTEXT                                   
040000         CALL FELLOG                                                      
040100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040200         CONTINUE                                                         
040300     END-SEARCH                                                           
040400     .                                                                    
