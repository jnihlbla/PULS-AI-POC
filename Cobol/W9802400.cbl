000010CBL TRUNC(BIN)                                                            
000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W9802400.                                                 
000400 AUTHOR.       KJELL ANDRE.                                               
000500     DATE-WRITTEN.  JULI 1985.                                            
000600                                                                          
000700     REMARKS.                                                             
000800     SKIP2                                                                
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET UTGÖR ISPF-INTERFACE TILL SOP.                        
001200*        VIA DETTA PROGRAM KAN MAN TITTA PÅ, OCH ÄNDRA STATUS             
001300*        PÅ PROCESSERNA I SOP.                                            
001400*        MAN KAN OCKSÅ GÖRA TEMPORÄRA JCL-ÄNDRINGAR FÖR EN                
001500*        PROCESS.                                                         
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200 WORKING-STORAGE SECTION.                                                 
002300*    -COPY WY2000W1                                                       
002400     SKIP3                                                                
240000*                                                                         
250000 77  PROGRAM-NAMN                PIC X(8)   VALUE 'W9802400'.             
260000     SKIP2                                                                
270000 01  JA                          PIC X       VALUE 'J'.                   
280000 01  NEJ                         PIC X       VALUE 'N'.                   
290000 01  PASSIVE-STATUS              PIC X       VALUE 'P'.                   
300000 01  WAITING-STATUS              PIC X       VALUE 'W'.                   
310000 01  STARTED-STATUS              PIC X       VALUE 'S'.                   
320000 01  ENDED-STATUS                PIC X       VALUE 'E'.                   
330000                                                                          
340000 01  SOP-OPPNAD                  PIC X       VALUE 'N'.                   
350000 01  SW-HELD                     PIC X       VALUE 'N'.                   
360000 01  SW-ABENDED                  PIC X       VALUE 'N'.                   
370000 01  JCL-ALLOCATED               PIC X       VALUE 'N'.                   
380000 01  AUTO-SWITCH                 PIC X       VALUE 'N'.                   
390000 01  SEL-ERROR                   PIC X       VALUE 'N'.                   
400000                                                                          
410029 01  SAVE-RC                     PIC S9(4)   COMP.                        
420029 01  PTR                         PIC S9(4)   COMP.                        
430013 01  TALLY-RAEKN                 PIC S9(4)   COMP.                        
440013 01  TEMP-STRING                 PIC X(30).                               
450000                                                                          
460000 01  ANTAL-VARV                  PIC S9(4)   COMP.                        
470000 01  ANTAL-RADER                 PIC S9(4)   COMP.                        
480000                                                                          
490000 01  AUTO-PERIOD                 PIC S9(9) COMP VALUE +6000.              
500000 01  ORD1                        PIC X(10).                               
510000 01  ORD2                        PIC X(10).                               
520000 01  NNX                         PIC XXX  JUSTIFIED RIGHT.                
530000 01  NN   REDEFINES NNX          PIC 999.                                 
540000     EJECT                                                                
550000 01  TID-ZONAT               PIC 9(4).                                    
560000 01  FILLER REDEFINES TID-ZONAT.                                          
570000     03  TID-HH              PIC XX.                                      
580000     03  TID-MM              PIC XX.                                      
590000                                                                          
600000 01  JOBB-NAMN.                                                           
610000     03  JOBB-NAMN-1-4       PIC X(4).                                    
620000     03  JOBB-NAMN-5         PIC X.                                       
630000     03  JOBB-NAMN-6-8       PIC X(3).                                    
640000                                                                          
650000     EJECT                                                                
660000 01  DYNAMISKA-SUBPROGRAM.                                                
670001     03  W009WAIT                PIC X(8)   VALUE 'W009WAIT'.             
680000     03  ISPLINK                 PIC X(8)   VALUE 'ISPLINK'.              
690001     03  W980WSPC                PIC X(8)   VALUE 'W980WSPC'.             
700000     03  W9802000                PIC X(8)   VALUE 'W9802000'.             
710000     EJECT                                                                
720000 01  VAR-LIST.                                                            
730000     03  ZCMD                    PIC X(40).                               
740000     03  ZSCREEN                 PIC X.                                   
750000     03  ZERRMSG                 PIC X(8).                                
760000     03  CRP                     PIC S9(9) COMP SYNC.                     
770000     03  FILLER                  PIC X(4).                                
780000     03  XDSN                    PIC X(44).                               
790000     03  XMSGCODE                PIC 999.                                 
800000     03  FUNC                    PIC X(4).                                
810000     03  SPROCESS                PIC X(10).                               
820000     03  XPROCESS                PIC X(10).                               
830000     03  SDSN                    PIC X(44).                               
840000     03  JDSN                    PIC X(44).                               
850000     03  JDSN2                   PIC X(44).                               
860000     03  TMPDSN                  PIC X(44).                               
870000     03  NULLPDS                 PIC X(44).                               
880000     03  ADATE                   PIC 9(6).                                
890000     03  MINSNR                  PIC X.                                   
900000     03  MAXTYP                  PIC X(3).                                
910000     03  WPROCESS                PIC X(10).                               
920000     03  WMTYP                   PIC X.                                   
930000     03  XMTYP                   PIC X.                                   
940000     03  WSTATUS                 PIC X.                                   
950000     03  WHOLD                   PIC X.                                   
960000     03  WATTN                   PIC X(30).                               
970000     03  WLADATE                 PIC 9(6).                                
980000     03  WLEDATE                 PIC 9(6).                                
990000     03  WLSTIME                 PIC 9(4).                                
000000     03  WLETIME                 PIC 9(4).                                
010000     03  WLXTIME                 PIC ZZZ9.                                
020000     03  WMXTIME                 PIC ZZZ9.                                
030024     03  WODATE                  PIC 9(6).                                
040024     03  WCATALOG                PIC X(200).                              
050000     03  WAQ                     PIC X(100).                              
060000     03  WDQ                     PIC X(100).                              
070000     03  WPRED                   PIC X(100).                              
080000     03  WSUCC                   PIC X(100).                              
090000     03  WRES                    PIC X(100).                              
100000     03  WSTYPE                  PIC X(4).                                
110000     03  WPARENT                 PIC X(10).                               
120000     03  WPRIO                   PIC X.                                   
130000     03  WVOUT                   PIC X.                                   
140024     03  WSYMB                   PIC X(1000).                             
150024     03  WSYMBOL                 PIC X(960).                              
160000     03  WABJOB                  PIC X(8).                                
170000     03  WTJCLD                  PIC S9(9) COMP SYNC.                     
180000     03  WADD                    PIC X(3).                                
190000     03  ZTDTOP                  PIC S9(9) COMP SYNC.                     
200000     03  TPROCESS                PIC X(10).                               
210000     03  TPARENT                 PIC X(20).                               
220025     03  TSEL                    PIC X(2).                                
230000     03  TMTYP                   PIC X(3).                                
240000     03  TNOTE                   PIC X(30).                               
250000     03  TATTN                   PIC X(30).                               
260005     03  TRES                    PIC X(3).                                
270005     03  TSTATUS                 PIC X(8).                                
280000     03  TSNR                    PIC X.                                   
290000     03  TSTYP                   PIC X(4).                                
300024     03  TODATE                  PIC 9(6).                                
310024     03  TOSTATUS                PIC X(8).                                
320024     03  TANR                    PIC -9(5).                               
330000     03  CDATUM                  PIC 9(6).                                
340024     03  CCATALOG                PIC X(200).                              
350000     03  CAQ                     PIC X(100).                              
360000     03  CPQ                     PIC X(100).                              
370000     03  CONF                    PIC X(10).                               
380000     03  JTMPDUR                 PIC 9(6).                                
390000     03  SOPJOBST                PIC X.                                   
400032     03  BACKPROC                PIC X(8).                                
410032     03  BACKPSB                 PIC X(8).                                
420033     03  ZZSESSID                PIC X(8).                                
430032                                                                          
440000     03  SYMB-IX                 PIC S9(4).                               
450000     03  P-TRUNK4                PIC X(4).                                
460000     03  TMP-ATTN                PIC X(30).                               
470000     03  TMP-ATTN-1-5 REDEFINES TMP-ATTN                                  
480000                                 PIC X(5).                                
490000     EJECT                                                                
500000 01  NAME-LIST.                                                           
510000     03 N-ZCMD                   PIC X(8)    VALUE 'ZCMD    '.            
520000     03 N-ZSCREEN                PIC X(8)    VALUE 'ZSCREEN '.            
530000     03 N-ZERRMSG                PIC X(8)    VALUE 'ZERRMSG '.            
540000     03 N-CRP                    PIC X(8)    VALUE 'CRP     '.            
550000     03 N-XDSN                   PIC X(8)    VALUE 'XDSN    '.            
560000     03 N-XMSGCODE               PIC X(8)    VALUE 'XMSGCODE'.            
570000     03 N-FUNC                   PIC X(8)    VALUE 'FUNC    '.            
580000     03 N-SPROCESS               PIC X(8)    VALUE 'SPROCESS'.            
590000     03 N-XPROCESS               PIC X(8)    VALUE 'XPROCESS'.            
600000     03 N-SDSN                   PIC X(8)    VALUE 'SDSN    '.            
610000     03 N-JDSN                   PIC X(8)    VALUE 'JDSN    '.            
620000     03 N-JDSN2                  PIC X(8)    VALUE 'JDSN2   '.            
630000     03 N-TMPDSN                 PIC X(8)    VALUE 'TMPDSN  '.            
640000     03 N-NULLPDS                PIC X(8)    VALUE 'NULLPDS '.            
650000     03 N-ADATE                  PIC X(8)    VALUE 'ADATE   '.            
660000     03 N-MINSNR                 PIC X(8)    VALUE 'MINSNR  '.            
670000     03 N-MAXTYP                 PIC X(8)    VALUE 'MAXTYP  '.            
680000     03 N-WPROCESS               PIC X(8)    VALUE 'WPROCESS'.            
690000     03 N-WMTYP                  PIC X(8)    VALUE 'WMTYP   '.            
700000     03 N-XMTYP                  PIC X(8)    VALUE 'XMTYP   '.            
710000     03 N-WSTATUS                PIC X(8)    VALUE 'WSTATUS '.            
720000     03 N-WHOLD                  PIC X(8)    VALUE 'WHOLD   '.            
730000     03 N-WATTN                  PIC X(8)    VALUE 'WATTN   '.            
740000     03 N-WLADATE                PIC X(8)    VALUE 'WLADATE '.            
750000     03 N-WLEDATE                PIC X(8)    VALUE 'WLEDATE '.            
760000     03 N-WLSTIME                PIC X(8)    VALUE 'WLSTIME '.            
770000     03 N-WLETIME                PIC X(8)    VALUE 'WLETIME '.            
780000     03 N-WLXTIME                PIC X(8)    VALUE 'WLXTIME '.            
790000     03 N-WMXTIME                PIC X(8)    VALUE 'WMXTIME '.            
800024     03 N-WODATE                 PIC X(8)    VALUE 'WODATE '.             
810000     03 N-WCATALOG               PIC X(8)    VALUE 'WCATALOG'.            
820000     03 N-WAQ                    PIC X(8)    VALUE 'WAQ     '.            
830000     03 N-WDQ                    PIC X(8)    VALUE 'WDQ     '.            
840000     03 N-WPRED                  PIC X(8)    VALUE 'WPRED   '.            
850000     03 N-WSUCC                  PIC X(8)    VALUE 'WSUCC   '.            
860000     03 N-WRES                   PIC X(8)    VALUE 'WRES    '.            
870000     03 N-WSTYPE                 PIC X(8)    VALUE 'WSTYPE  '.            
880000     03 N-WPARENT                PIC X(8)    VALUE 'WPARENT '.            
890000     03 N-WPRIO                  PIC X(8)    VALUE 'WPRIO   '.            
900000     03 N-WVOUT                  PIC X(8)    VALUE 'WVOUT   '.            
910032     03 N-WSYMB                  PIC X(8)    VALUE 'WSYMB   '.            
920032     03 N-WSYMBOL                PIC X(8)    VALUE 'WSYMBOL '.            
930032     03 N-WABJOB                 PIC X(8)    VALUE 'WABJOB  '.            
940000     03 N-WTJCLD                 PIC X(8)    VALUE 'WTJCLD  '.            
950032     03 N-ADD                    PIC X(8)    VALUE 'ADD     '.            
960000     03 N-ZTDTOP                 PIC X(8)    VALUE 'ZTDTOP  '.            
970000     03 N-TPROCESS               PIC X(8)    VALUE 'TPROCESS'.            
980000     03 N-TPARENT                PIC X(8)    VALUE 'TPARENT '.            
990000     03 N-TSEL                   PIC X(8)    VALUE 'TSEL    '.            
000000     03 N-TMTYP                  PIC X(8)    VALUE 'TMTYP   '.            
010000     03 N-TNOTE                  PIC X(8)    VALUE 'TNOTE   '.            
020000     03 N-TATTN                  PIC X(8)    VALUE 'TATTN   '.            
030005     03 N-TRES                   PIC X(8)    VALUE 'TRES    '.            
040000     03 N-TSTATUS                PIC X(8)    VALUE 'TSTATUS '.            
050000     03 N-TSNR                   PIC X(8)    VALUE 'TSNR    '.            
060000     03 N-TSTYP                  PIC X(8)    VALUE 'TSTYP   '.            
070024     03 N-TODATE                 PIC X(8)    VALUE 'TODATE  '.            
080024     03 N-TOSTATUS               PIC X(8)    VALUE 'TOSTATUS'.            
090024     03 N-TANR                   PIC X(8)    VALUE 'TANR    '.            
100000     03 N-CDATUM                 PIC X(8)    VALUE 'CDATUM  '.            
110000     03 N-CCATALOG               PIC X(8)    VALUE 'CCATALOG'.            
120000     03 N-CAQ                    PIC X(8)    VALUE 'CAQ     '.            
130000     03 N-CPQ                    PIC X(8)    VALUE 'CPQ     '.            
140000     03 N-CONF                   PIC X(8)    VALUE 'CONF    '.            
150000     03 N-JTMPDUR                PIC X(8)    VALUE 'JTMPDUR '.            
160000     03 N-SOPJOBST               PIC X(8)    VALUE 'SOPJOBST'.            
170032     03 N-BACKPROC               PIC X(8)    VALUE 'BACKPROC'.            
180032     03 N-BACKPSB                PIC X(8)    VALUE 'BACKPSB '.            
190033     03 N-ZZSESSID               PIC X(8)    VALUE 'ZZSESSID'.            
200000     EJECT                                                                
210000 01  LENG-LIST SYNC.                                                      
220000     03 L-ZCMD                   PIC S9(9) COMP  VALUE +40.               
230000     03 L-ZSCREEN                PIC S9(9) COMP  VALUE +1.                
240000     03 L-ZERRMSG                PIC S9(9) COMP  VALUE +8.                
250000     03 L-CRP                    PIC S9(9) COMP  VALUE +8.                
260000     03 L-XDSN                   PIC S9(9) COMP  VALUE +44.               
270000     03 L-XMSGCODE               PIC S9(9) COMP  VALUE +3.                
280000     03 L-FUNC                   PIC S9(9) COMP  VALUE +4.                
290000     03 L-SPROCESS               PIC S9(9) COMP  VALUE +10.               
300000     03 L-XPROCESS               PIC S9(9) COMP  VALUE +10.               
310000     03 L-SDSN                   PIC S9(9) COMP  VALUE +44.               
320000     03 L-JDSN                   PIC S9(9) COMP  VALUE +44.               
330000     03 L-JDSN2                  PIC S9(9) COMP  VALUE +44.               
340000     03 L-NULLPDS                PIC S9(9) COMP  VALUE +44.               
350000     03 L-TMPDSN                 PIC S9(9) COMP  VALUE +44.               
360000     03 L-ADATE                  PIC S9(9) COMP  VALUE +6.                
370000     03 L-MINSNR                 PIC S9(9) COMP  VALUE +1.                
380000     03 L-MAXTYP                 PIC S9(9) COMP  VALUE +3.                
390000     03 L-WPROCESS               PIC S9(9) COMP  VALUE +10.               
400000     03 L-WMTYP                  PIC S9(9) COMP  VALUE +1.                
410000     03 L-XMTYP                  PIC S9(9) COMP  VALUE +1.                
420000     03 L-WSTATUS                PIC S9(9) COMP  VALUE +1.                
430000     03 L-WHOLD                  PIC S9(9) COMP  VALUE +1.                
440000     03 L-WATTN                  PIC S9(9) COMP  VALUE +30.               
450000     03 L-WLADATE                PIC S9(9) COMP  VALUE +6.                
460000     03 L-WLEDATE                PIC S9(9) COMP  VALUE +6.                
470000     03 L-WLSTIME                PIC S9(9) COMP  VALUE +4.                
480000     03 L-WLETIME                PIC S9(9) COMP  VALUE +4.                
490000     03 L-WLXTIME                PIC S9(9) COMP  VALUE +4.                
500000     03 L-WMXTIME                PIC S9(9) COMP  VALUE +4.                
510024     03 L-WODATE                 PIC S9(9) COMP  VALUE +6.                
520024     03 L-WCATALOG               PIC S9(9) COMP  VALUE +200.              
530000     03 L-WAQ                    PIC S9(9) COMP  VALUE +100.              
540000     03 L-WDQ                    PIC S9(9) COMP  VALUE +100.              
550000     03 L-WPRED                  PIC S9(9) COMP  VALUE +100.              
560000     03 L-WSUCC                  PIC S9(9) COMP  VALUE +100.              
570000     03 L-WRES                   PIC S9(9) COMP  VALUE +100.              
580000     03 L-WSTYPE                 PIC S9(9) COMP  VALUE +4.                
590000     03 L-WPARENT                PIC S9(9) COMP  VALUE +10.               
600000     03 L-WPRIO                  PIC S9(9) COMP  VALUE +1.                
610000     03 L-WVOUT                  PIC S9(9) COMP  VALUE +1.                
620032     03 L-WSYMB                  PIC S9(9) COMP  VALUE +1000.             
630032     03 L-WSYMBOL                PIC S9(9) COMP  VALUE +960.              
640032     03 L-WABJOB                 PIC S9(9) COMP  VALUE +8.                
650000     03 L-WTJCLD                 PIC S9(9) COMP  VALUE +4.                
660032     03 L-ADD                    PIC S9(9) COMP  VALUE +3.                
670000     03 L-ZTDTOP                 PIC S9(9) COMP  VALUE +4.                
680000     03 L-TPROCESS               PIC S9(9) COMP  VALUE +10.               
690000     03 L-TPARENT                PIC S9(9) COMP  VALUE +20.               
700025     03 L-TSEL                   PIC S9(9) COMP  VALUE +2.                
710000     03 L-TMTYP                  PIC S9(9) COMP  VALUE +3.                
720000     03 L-TNOTE                  PIC S9(9) COMP  VALUE +30.               
730000     03 L-TATTN                  PIC S9(9) COMP  VALUE +30.               
740005     03 L-TRES                   PIC S9(9) COMP  VALUE +3.                
750000     03 L-TSTATUS                PIC S9(9) COMP  VALUE +8.                
760000     03 L-TSNR                   PIC S9(9) COMP  VALUE +1.                
770000     03 L-TSTYP                  PIC S9(9) COMP  VALUE +4.                
780024     03 L-TODATE                 PIC S9(9) COMP  VALUE +6.                
790024     03 L-TOSTATUS               PIC S9(9) COMP  VALUE +8.                
800024     03 L-TANR                   PIC S9(9) COMP  VALUE +6.                
810000     03 L-CDATUM                 PIC S9(9) COMP  VALUE +6.                
820024     03 L-CCATALOG               PIC S9(9) COMP  VALUE +200.              
830000     03 L-CAQ                    PIC S9(9) COMP  VALUE +100.              
840000     03 L-CPQ                    PIC S9(9) COMP  VALUE +100.              
850000     03 L-CONF                   PIC S9(9) COMP  VALUE +10.               
860000     03 L-JTMPDUR                PIC S9(9) COMP  VALUE +6.                
870000     03 L-SOPJOBST               PIC S9(9) COMP  VALUE +1.                
880032     03 L-BACKPROC               PIC S9(9) COMP  VALUE +8.                
890032     03 L-BACKPSB                PIC S9(9) COMP  VALUE +8.                
900033     03 L-ZZSESSID               PIC S9(9) COMP  VALUE +8.                
910000     EJECT                                                                
920000 01  ISP-SELECT                  PIC X(8)    VALUE 'SELECT  '.            
930000 01  ISP-DISPLAY                 PIC X(8)    VALUE 'DISPLAY '.            
940000 01  ISP-EDIT                    PIC X(8)    VALUE 'EDIT    '.            
950000 01  ISP-CONTROL                 PIC X(8)    VALUE 'CONTROL '.            
960000 01  ISP-LOCK                    PIC X(8)    VALUE 'LOCK    '.            
970000 01  SAVE                        PIC X(8)    VALUE 'SAVE    '.            
980000 01  RESTORE                     PIC X(8)    VALUE 'RESTORE '.            
990000 01  SETMSG                      PIC X(8)    VALUE 'SETMSG  '.            
000000 01  VDEFINE                     PIC X(8)    VALUE 'VDEFINE '.            
010000 01  VRESET                      PIC X(8)    VALUE 'VRESET  '.            
020000 01  VGET                        PIC X(8)    VALUE 'VGET    '.            
030000 01  VPUT                        PIC X(8)    VALUE 'VPUT    '.            
040000 01  VREPLACE                    PIC X(8)    VALUE 'VREPLACE'.            
050000 01  TBCREATE                    PIC X(8)    VALUE 'TBCREATE'.            
060000 01  TBEND                       PIC X(8)    VALUE 'TBEND   '.            
070000 01  TBDISPL                     PIC X(8)    VALUE 'TBDISPL '.            
080000 01  TBADD                       PIC X(8)    VALUE 'TBADD   '.            
090000 01  TBTOP                       PIC X(8)    VALUE 'TBTOP   '.            
100000 01  TBGET                       PIC X(8)    VALUE 'TBGET   '.            
110000 01  TBPUT                       PIC X(8)    VALUE 'TBPUT   '.            
120000 01  TBSKIP                      PIC X(8)    VALUE 'TBSKIP  '.            
130028 01  ADDPOP                      PIC X(8)    VALUE 'ADDPOP  '.            
140028 01  REMPOP                      PIC X(8)    VALUE 'REMPOP  '.            
150000                                                                          
160000 01  CHAR                      PIC X(8)   VALUE 'CHAR    '.               
170000 01  FIXED                     PIC X(8)   VALUE 'FIXED   '.               
180000 01  VDEFINE-OPT               PIC X(16)  VALUE '(COPY NOBSCAN)'.         
190000 01  NOWRITE                   PIC X(8)   VALUE 'NOWRITE '.               
200000 01  NOPARM                    PIC X      VALUE SPACE.                    
210025 01  ISP-SHARED                PIC X(8)    VALUE 'SHARED  '.              
220000 01  PROFILE                   PIC X(8)    VALUE 'PROFILE '.              
230000 01  ERRORS                    PIC X(8)    VALUE 'ERRORS  '.              
240000 01  ISP-RETURN                PIC X(8)    VALUE 'RETURN  '.              
250000 01  ISP-ENTER                 PIC X(8)    VALUE 'ENTER   '.              
260000 01  NONDISPL                  PIC X(8)    VALUE 'NONDISPL'.              
270000                                                                          
280000 01  GRUND-PANEL               PIC X(8)   VALUE 'SOPF    '.               
290000 01  INFO-PANEL                PIC X(8)   VALUE 'SOPFPI  '.               
300024 01  INFO2-PANEL               PIC X(8)   VALUE 'SOPFQOI '.               
310000 01  MSL-PANEL                 PIC X(8)   VALUE 'SOPFMSL '.               
320024 01  OQ-PANEL                  PIC X(8)   VALUE 'SOPFMQO '.               
330000 01  SYMB-PANEL                PIC X(8)   VALUE 'SOPSYMB '.               
340000 01  SYMB-PANEL2               PIC X(8)   VALUE 'SOPSYMB2'.               
350000 01  CONFIRM-PANEL             PIC X(8)   VALUE 'SOPFCONF'.               
360000 01  KALENDER-PANEL            PIC X(8)   VALUE 'SOPFCAL '.               
370000 01  TEMP-PANEL-1              PIC X(8)   VALUE 'SOPFTMP1'.               
380000 01  TEMP-PANEL-2              PIC X(8)   VALUE 'SOPFTMP2'.               
390034 01  BACKOUT-PANEL             PIC X(8)   VALUE 'SOPBACK '.               
400000                                                                          
410000 01  ERROR-ALLOC-FAIL          PIC X(8)   VALUE 'SOP101  '.               
420000 01  ERROR-FUNC-FAIL           PIC X(8)   VALUE 'SOP102  '.               
430000 01  ERROR-NO-SUB              PIC X(8)   VALUE 'SOP103  '.               
440000 01  ERROR-ILLEGAL-SEL         PIC X(8)   VALUE 'SOP104  '.               
450000 01  ERROR-NO-SUIT-SUB         PIC X(8)   VALUE 'SOP105  '.               
460000 01  ERROR-NO-STARTED          PIC X(8)   VALUE 'SOP106  '.               
470000 01  ERROR-ILLEGAL-CMD         PIC X(8)   VALUE 'SOP107  '.               
480000 01  ERROR-PROC-NOT-FOUND      PIC X(8)   VALUE 'SOP109  '.               
490000 01  ERROR-TEMP-NOT-ALLOWED    PIC X(8)   VALUE 'SOP110  '.               
500000 01  MSG-TEMP-DELETED          PIC X(8)   VALUE 'SOP111  '.               
510000 01  ERROR-RETRIEVE-FAIL       PIC X(8)   VALUE 'SOP112  '.               
520000 01  ERROR-STORE-FAIL          PIC X(8)   VALUE 'SOP113  '.               
530000 01  ERROR-NO-SUIT-TYPE        PIC X(8)   VALUE 'SOP115  '.               
540000 01  MSG-AUTO-INTERRUPTED      PIC X(8)   VALUE 'SOP117  '.               
550000 01  MSG-TEMP-ACTIVATED        PIC X(8)   VALUE 'SOP118  '.               
560000 01  NO-SYS-HOLD-Q             PIC X(8)   VALUE 'SOP119  '.               
570024 01  ERROR-NO-ORDER-QUEUE      PIC X(8)   VALUE 'SOP120  '.               
580000 01  ERROR-ILLEGAL-MTYP        PIC X(8)   VALUE 'SOP121  '.               
590000     EJECT                                                                
600000 01  TB-NAMN.                                                             
610000     03  FILLER                PIC X(7)   VALUE 'SOPXTAB'.                
620000     03  TB-NR                 PIC 9.                                     
630000                                                                          
640000 01  TB-KEYNAMES               PIC X      VALUE SPACE.                    
650000                                                                          
660005 01  TB-NAMES                  PIC X(55)  VALUE                           
670005     '(TPROCESS TMTYP TSTATUS TNOTE TATTN TSNR TSTYP TRES)'.              
680024     EJECT                                                                
690024 01  TBOQ-NAMN.                                                           
700024     03  FILLER                PIC X(8)   VALUE 'SOPOQTAB'.               
710024                                                                          
720024 01  TBOQ-KEYNAMES             PIC X(20)  VALUE                           
730024     '(TANR)'.                                                            
740024                                                                          
750024 01  TBOQ-NAMES                PIC X(50)  VALUE                           
760024     '(TODATE TOSTATUS)'.                                                 
770000     EJECT                                                                
780000 01  SPAR-SDSN                 PIC X(44).                                 
790000 01  SPAR-JDSN                 PIC X(44).                                 
800000 01  SPAR-JDSN2                PIC X(44).                                 
810000                                                                          
820000                                                                          
830000 01  WS-SEL                    PIC X.                                     
840000                                                                          
850000 01  L-FREE-CMD                PIC S9(9) COMP  VALUE +40.                 
860000 01  FREE-CMD.                                                            
870000     03  FILLER                PIC X(12)                                  
880000                               VALUE 'CMD(FREE DD('.                      
890000     03  DDNAMN.                                                          
900000         05 SOPDDN1            PIC X(8) VALUE 'XSOPDD1 '.                 
910000         05 FILLER             PIC X    VALUE SPACE.                      
920000         05 PDSDDN1            PIC X(8) VALUE 'XPDSDD1 '.                 
930000         05 FILLER             PIC X    VALUE SPACE.                      
940000         05 PDSDDN2            PIC X(8) VALUE 'XPDSDD2 '.                 
950000     03  FILLER                PIC X(2) VALUE '))'.                       
960000                                                                          
970000 01  L-ALLOC-CMD               PIC S9(9) COMP  VALUE +84.                 
980000 01  ALLOC-CMD.                                                           
990000     03  FILLER                PIC X(13)   VALUE 'CMD(ALLOC DD('.         
000000     03  ALLOC-DDN             PIC X(8).                                  
010000     03  FILLER                PIC X(6)    VALUE ') DSN('.                
020000     03  ALLOC-DSN             PIC X(44).                                 
030000     03  FILLER                PIC X(13)   VALUE  ') SHR REUSE )'.        
040000                                                                          
050000 01  L-TEMP-HAMTA-CMD          PIC S9(9) COMP  VALUE +78.                 
060000 01  TEMP-HAMTA-CMD.                                                      
070000     03  FILLER                PIC X(13)                                  
080000         VALUE 'CMD(%SOPTMPR '.                                           
090000     03  TEMP-HAMTA-DDNAMN     PIC X(8).                                  
100000     03  FILLER                PIC X   VALUE ' '.                         
110000     03  TEMP-HAMTA-DSNAME     PIC X(44).                                 
120000     03  FILLER                PIC X   VALUE ' '.                         
130000     03  TEMP-HAMTA-IDPROCESS  PIC X(10).                                 
140000     03  FILLER                PIC X   VALUE ')'.                         
150000                                                                          
160000 01  L-TEMP-LAGRA-CMD          PIC S9(9) COMP  VALUE +78.                 
170000 01  TEMP-LAGRA-CMD.                                                      
180000     03  FILLER                PIC X(13)                                  
190000         VALUE 'CMD(%SOPTMPS '.                                           
200000     03  TEMP-LAGRA-DDNAMN     PIC X(8).                                  
210000     03  FILLER                PIC X   VALUE ' '.                         
220000     03  TEMP-LAGRA-DSNAME     PIC X(44).                                 
230000     03  FILLER                PIC X   VALUE ' '.                         
240000     03  TEMP-LAGRA-IDPROCESS  PIC X(10).                                 
250000     03  FILLER                PIC X   VALUE ')'.                         
260000                                                                          
270000 01  L-STATUS-CMD              PIC S9(9) COMP  VALUE +24.                 
280000 01  STATUS-CMD.                                                          
290000     03  FILLER                PIC X(14)                                  
300000         VALUE 'CMD(%SOPJOBST '.                                          
310000     03  STATUS-JOBNAMN.                                                  
320000         05  FILLER              PIC X(4).                                
330000         05  STATUS-JOBNAMN-POS5 PIC X.                                   
340000         05  FILLER              PIC X(3).                                
350000     03  FILLER                PIC XX  VALUE ' )'.                        
360000                                                                          
370000 01  L-DISPLAY-SDSF-CMD        PIC S9(9) COMP  VALUE +13.                 
380000 01  DISPLAY-SDSF-CMD.                                                    
390000     03  FILLER                PIC X(13)                                  
400000         VALUE 'CMD(%ISFSOPQ)'.                                           
410000                                                                          
420044 01  W-BACKPROC-POS6           PIC X.                                     
421044                                                                          
430033 01  BACKOUT-TESYMBV.                                                     
440033     03  FILLER                PIC X(9)   VALUE ' BACKJOB('.              
450033     03  SYMB-BACKJOB          PIC X(10)   VALUE SPACE.                   
460033     03  FILLER                PIC X(9)   VALUE 'BACKUSER('.              
470033     03  SYMB-BACKUSER         PIC X(10)   VALUE SPACE.                   
480033     03  FILLER                PIC X(9)   VALUE 'BACKPROC('.              
490033     03  SYMB-BACKPROC         PIC X(10)   VALUE SPACE.                   
500033     03  FILLER                PIC X(9)   VALUE ' BACKPSB('.              
510033     03  SYMB-BACKPSB          PIC X(10)   VALUE SPACE.                   
520000     EJECT                                                                
530004*01  -COPY W98020                                                         
540000     EJECT                                                                
550001*---------------------- PARAMETRAR VID ANROP AV W980WSPC                  
560000                                                                          
570000 01  FUNKTIONSKODER.                                                      
580000     03  FGETF               PIC X(4)  VALUE 'GETF'.                      
590000     03  FGETN               PIC X(4)  VALUE 'GETN'.                      
600000     03  FSAVE               PIC X(4)  VALUE 'SAVE'.                      
610000     03  FQUIT               PIC X(4)  VALUE 'QUIT'.                      
620000     03  FTEST               PIC X(4)  VALUE 'TEST'.                      
630000     03  FDELK               PIC X(4)  VALUE 'DELK'.                      
640000     03  FSET                PIC X(4)  VALUE 'SET '.                      
650000                                                                          
660000 01  WSRKOD                  PIC X.                                       
670000                                                                          
680000*---------------------  GENERELLA ARBETS-PARAMETRAR                       
690000 01  NAMN-PARM.                                                           
700000     03  NAMN-LENGD          PIC S9(4)  COMP.                             
710000     03  NAMN-VAERDE         PIC X(20).                                   
720000                                                                          
730000 01  ATTR-PARM.                                                           
740000     03  ATTR-LENGD          PIC S9(4)  COMP.                             
750000     03  ATTR-VAERDE         PIC X(20).                                   
760000                                                                          
770000 01  DATA-PARM.                                                           
780000     03  DATA-LENGD          PIC S9(4)  COMP.                             
790000     03  DATA-VAERDE         PIC X(40).                                   
800000                                                                          
810024*--------- DATA-PARAMETER TILL ATTRIBUT "INF"                             
820024 01  INFO-VAERDE-PARM.                                                    
830024*    03   -COPY W980INF                                                   
840024                                                                          
850000*---------------------  FÄLT FÖR HANTERING AV DATUM-DATA                  
860000 01  DATUM-ZONAT             PIC 9(6).                                    
870000 01  DATUM-PARM.                                                          
880000     03  FILLER              PIC S9(4)  COMP  VALUE +6.                   
890000     03  DATUM-PACKAT        PIC S9(7)  COMP-3.                           
900000                                                                          
910000*---------------------  FÄLT FÖR HANTERING AV DATUM-DATA                  
920000 01  NUDAT-PARM.                                                          
930000     03  FILLER              PIC S9(4)  COMP  VALUE +6.                   
940000     03  NUDAT               PIC S9(7)  COMP-3.                           
950000                                                                          
960024*-------- AKTIVERINGSDATUM                                                
970024 01  BEGAERD-TIAPDAT-PARM.                                                
980024     03  FILLER              PIC S9(4)   COMP VALUE +6.                   
990024     03  BEGAERD-TIAPDAT     PIC S9(7)   COMP-3.                          
000024                                                                          
010024*-------- AKTIVERINGSDATUM + ORDERNUMMER                                  
020024 01  BEG-TIAPDAT-ANR-PARM.                                                
030024     03  BEG-DAT-ANR-LENGD   PIC S9(4)   COMP VALUE +8.                   
040024     03  BEG-TIAPDAT         PIC S9(7)   COMP-3.                          
050024     03  P-ANR               PIC S9(4)   COMP.                            
060024                                                                          
070000*---------------------  STATUS PÅ DEN AKTUELLA SOPBASEN                   
080000 01  DB-STATUS-VAERDE-PARM.                                               
090000     03  FILLER              PIC S9(4)  COMP  VALUE +6.                   
100025     03  DB-STATUS-VAERDE    PIC X(4).                                    
110000     EJECT                                                                
120000*---------------------  PROCESS SOM BEARBETAS FÖR TILLFÄLLET.             
130000 01  P-PARM.                                                              
140000     03  P-LENGD             PIC S9(4)  COMP.                             
150000     03  P-IDPROCESS         PIC X(10).                                   
160000                                                                          
170000*----------------------- TABELL FÖR PROCESS HIERARKI VID EXPAND           
180000 01  HIX                     PIC S9(9) COMP.                              
190000 01  HIX-FULL                PIC S9(9) COMP VALUE +10.                    
200000 01  HIERARKI-TABELL.                                                     
210000     03  TAB-RAD  OCCURS 11.                                              
220000       05  H-TBDISPL-RC        PIC S9(4)  COMP.                           
230000       05  H-PARM.                                                        
240000           07  H-LENGD         PIC S9(4)  COMP.                           
250000           07  H-IDPROCESS     PIC X(10).                                 
260024     SKIP2                                                                
270024 01  TBOQDISPL-RC              PIC S9(4)  COMP.                           
280000     EJECT                                                                
290000 01  CONTAINS-PARM.                                                       
300000     03  FILLER            PIC S9(4) COMP VALUE +5.                       
310000     03  FILLER            PIC X(3)       VALUE 'CON'.                    
320000                                                                          
330000 01  STARTED-LIST-PARM.                                                   
340000     03  FILLER            PIC S9(4) COMP VALUE +4.                       
350000     03  FILLER            PIC X(2)       VALUE 'SL'.                     
360000                                                                          
370000 01  NULL-PARM.                                                           
380000     03  FILLER            PIC S9(4) COMP VALUE +2.                       
390000                                                                          
400000 01  KALENDER-PARM.                                                       
410000     03  FILLER            PIC S9(4) COMP VALUE +7.                       
420000     03  FILLER            PIC X(5)       VALUE '*CAL*'.                  
430000                                                                          
440000 01  NUDAT-NAMN-PARM.                                                     
450000     03  FILLER            PIC S9(4) COMP VALUE +6.                       
460000     03  FILLER            PIC X(4)       VALUE 'CDAT'.                   
470000                                                                          
480000 01  CATALOG-PARM.                                                        
490000     03  FILLER            PIC S9(4) COMP VALUE +5.                       
500000     03  FILLER            PIC X(3)       VALUE 'CAT'.                    
510000                                                                          
520000 01  NOT-CATALOG-PARM.                                                    
530000     03  FILLER            PIC S9(4) COMP VALUE +6.                       
540000     03  FILLER            PIC X(4)       VALUE 'NCAT'.                   
550000                                                                          
560000 01  ACT-QUEUE-PARM.                                                      
570000     03  FILLER            PIC S9(4) COMP VALUE +4.                       
580000     03  FILLER            PIC X(2)       VALUE 'AQ'.                     
590000                                                                          
600000 01  PASS-QUEUE-PARM.                                                     
610000     03  FILLER            PIC S9(4) COMP VALUE +4.                       
620000     03  FILLER            PIC X(2)       VALUE 'DQ'.                     
630000                                                                          
640000 01  DATUM-LIST-PARM.                                                     
650000     03  FILLER            PIC S9(4) COMP VALUE +6.                       
660000     03  FILLER            PIC X(4)       VALUE 'DATL'.                   
670000                                                                          
680000 01  STARTTYP-PARM.                                                       
690000     03  FILLER            PIC S9(4) COMP VALUE +6.                       
700000     03  FILLER            PIC X(4)       VALUE 'STYP'.                   
710000                                                                          
720000 01  TEMP-VARAKTIGHET-PARM.                                               
730000     03  FILLER            PIC S9(4) COMP VALUE +6.                       
740000     03  FILLER            PIC X(4)       VALUE 'TEMP'.                   
750000                                                                          
760000 01  INFO-NAMN-PARM.                                                      
770000     03  FILLER            PIC S9(4) COMP VALUE +5.                       
780000     03  FILLER            PIC X(3)       VALUE 'INF'.                    
790000                                                                          
800000 01  MTYP-PARM.                                                           
810000     03  FILLER            PIC S9(4) COMP VALUE +6.                       
820000     03  FILLER            PIC X(4)       VALUE 'MTYP'.                   
830000                                                                          
840000 01  DB-STATUS-PARM.                                                      
850000     03  FILLER            PIC S9(4) COMP VALUE +8.                       
860000     03  FILLER            PIC X(6)       VALUE 'DBSTAT'.                 
870000                                                                          
880000 01  SOP-PARM.                                                            
890000     03  FILLER            PIC S9(4) COMP VALUE +7.                       
900000     03  FILLER            PIC X(5)       VALUE '*SOP*'.                  
910000     EJECT                                                                
920000 PROCEDURE DIVISION.                                                      
930000                                                                          
940000     PERFORM A-INIT                                                       
950000     CALL ISPLINK USING ISP-DISPLAY GRUND-PANEL ZERRMSG                   
960000     PERFORM UNTIL RETURN-CODE NOT < 8                                    
970000       CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY SAVE                    
980000       MOVE SPACE TO ZERRMSG                                              
990000       PERFORM B-ALLOCKERINGAR                                            
000000       EVALUATE  FUNC                                                     
010000         WHEN 'O' PERFORM C-SOP20                                         
020000         WHEN 'C' PERFORM C-SOP20                                         
030000         WHEN 'A' PERFORM C-SOP20                                         
040000         WHEN 'P' PERFORM C-SOP20                                         
050000         WHEN 'H' PERFORM C-SOP20                                         
060000         WHEN 'R' PERFORM C-SOP20                                         
070030         WHEN 'S' PERFORM C-SOP20                                         
080000         WHEN 'E' PERFORM C-SOP20                                         
090000         WHEN 'I' PERFORM C-SOP20                                         
100000         WHEN 'F' PERFORM C-SOP20                                         
110000         WHEN 'X' PERFORM D-EXPAND-PROCESS                                
120000         WHEN 'LS' PERFORM E-LIST-STARTED                                 
130000         WHEN 'K' PERFORM F-DISPLAY-KALENDER                              
140000         WHEN 'J' PERFORM G-TEMPANDRING-JCL                               
150000         WHEN 'V' PERFORM H-LAGRA-SYMBOLER                                
160000         WHEN 'Q' PERFORM I-DISPLAY-SDSF                                  
170024         WHEN 'OQ' PERFORM J-DISPLAY-ORDER-QUEUE                          
180032         WHEN 'B'  PERFORM K-ACTIVATE-BACKOUT                             
190000       END-EVALUATE                                                       
200000       CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE                 
210000       IF ZERRMSG NOT = SPACE AND MSG-TEMP-DELETED                        
220000         CALL ISPLINK USING ISP-DISPLAY NOPARM ZERRMSG                    
230000       ELSE                                                               
240000         CALL ISPLINK USING ISP-DISPLAY GRUND-PANEL ZERRMSG               
250000       END-IF                                                             
260000     END-PERFORM                                                          
270000     MOVE SPACE TO ZERRMSG                                                
280000     PERFORM Z-FINIT                                                      
290000                                                                          
300000     GOBACK                                                               
310000     .                                                                    
320000     EJECT                                                                
330000 A-INIT SECTION.                                                          
340000     SKIP2                                                                
350000     MOVE SPACE TO ZCMD                                                   
360004     MOVE JA TO SOP20-FLBATCH                                             
370000     CALL  ISPLINK USING VDEFINE N-ZCMD ZCMD CHAR                         
380000                         L-ZCMD VDEFINE-OPT                               
390000     IF RETURN-CODE NOT > 8                                               
400000       MOVE SPACE TO FUNC                                                 
410000       CALL ISPLINK USING VDEFINE N-FUNC FUNC CHAR                        
420000                  L-FUNC VDEFINE-OPT                                      
430000     END-IF                                                               
440000     IF RETURN-CODE NOT > 8                                               
450000       MOVE '0'   TO ZSCREEN                                              
460000       CALL ISPLINK USING VDEFINE N-ZSCREEN    ZSCREEN   CHAR             
470000                         L-ZSCREEN  VDEFINE-OPT                           
480000       CALL ISPLINK USING VGET N-ZSCREEN                                  
490000                                                                          
500000       MOVE ZSCREEN TO SOP20-IDDDPREFIX                                   
510000       INSPECT SOP20-IDDDPREFIX REPLACING ALL '0' BY 'X'                  
520000          '1' BY 'A' '2' BY 'B' '3' BY 'C' '4' BY 'D'                     
530000       MOVE SPACE TO SOPDDN1                                              
540000       STRING SOP20-IDDDPREFIX 'SOPDD1' DELIMITED BY SPACE                
550000         INTO SOPDDN1                                                     
560000       MOVE SPACE TO PDSDDN1                                              
570000       STRING SOP20-IDDDPREFIX 'PDSDD1' DELIMITED BY SPACE                
580000         INTO PDSDDN1                                                     
590000       MOVE SPACE TO PDSDDN2                                              
600000       STRING SOP20-IDDDPREFIX 'PDSDD2' DELIMITED BY SPACE                
610000         INTO PDSDDN2                                                     
620000     END-IF                                                               
630000                                                                          
640000     IF RETURN-CODE NOT > 8                                               
650000       MOVE 'SYSOUT' TO ALLOC-DDN                                         
660000       MOVE '*'      TO ALLOC-DSN                                         
670000       CALL ISPLINK USING ISP-SELECT L-ALLOC-CMD ALLOC-CMD                
680000       MOVE ZERO TO RETURN-CODE                                           
690000     END-IF                                                               
700000                                                                          
710000     IF RETURN-CODE NOT > 8                                               
720000       MOVE SPACE TO ZERRMSG                                              
730000       CALL  ISPLINK USING VDEFINE N-ZERRMSG    ZERRMSG  CHAR             
740000                           L-ZERRMSG  VDEFINE-OPT                         
750000     END-IF                                                               
760000     IF RETURN-CODE NOT > 8                                               
770000       MOVE ZERO TO CRP                                                   
780000       CALL  ISPLINK USING VDEFINE N-CRP        CRP      CHAR             
790000                           L-CRP      VDEFINE-OPT                         
800000     END-IF                                                               
810000     IF RETURN-CODE NOT > 8                                               
820000       MOVE SPACE TO XDSN                                                 
830000       CALL  ISPLINK USING VDEFINE N-XDSN       XDSN     CHAR             
840000                           L-XDSN     VDEFINE-OPT                         
850000     END-IF                                                               
860000     IF RETURN-CODE NOT > 8                                               
870000       MOVE ZERO  TO XMSGCODE                                             
880000       CALL  ISPLINK USING VDEFINE N-XMSGCODE XMSGCODE CHAR               
890000                           L-XMSGCODE VDEFINE-OPT                         
900000     END-IF                                                               
910000     IF RETURN-CODE NOT > 8                                               
920000       MOVE SPACE TO SPROCESS                                             
930000       CALL  ISPLINK USING VDEFINE N-SPROCESS SPROCESS CHAR               
940000                           L-SPROCESS VDEFINE-OPT                         
950000     END-IF                                                               
960000     IF RETURN-CODE NOT > 8                                               
970000       MOVE SPACE TO XPROCESS                                             
980000       CALL  ISPLINK USING VDEFINE N-XPROCESS XPROCESS CHAR               
990000                           L-XPROCESS VDEFINE-OPT                         
000000     END-IF                                                               
010000     IF RETURN-CODE NOT > 8                                               
020000       MOVE SPACE TO SDSN                                                 
030000       CALL  ISPLINK USING VDEFINE N-SDSN       SDSN     CHAR             
040000                           L-SDSN     VDEFINE-OPT                         
050000     END-IF                                                               
060000     IF RETURN-CODE NOT > 8                                               
070000       MOVE SPACE TO JDSN                                                 
080000       CALL  ISPLINK USING VDEFINE N-JDSN       JDSN     CHAR             
090000                           L-JDSN     VDEFINE-OPT                         
100000     END-IF                                                               
110000     IF RETURN-CODE NOT > 8                                               
120000       MOVE SPACE TO JDSN2                                                
130000       CALL  ISPLINK USING VDEFINE N-JDSN2      JDSN2    CHAR             
140000                           L-JDSN2    VDEFINE-OPT                         
150000     END-IF                                                               
160000     IF RETURN-CODE NOT > 8                                               
170000       MOVE SPACE TO TMPDSN                                               
180000       CALL  ISPLINK USING VDEFINE N-TMPDSN     TMPDSN   CHAR             
190000                           L-TMPDSN   VDEFINE-OPT                         
200000     END-IF                                                               
210000     IF RETURN-CODE NOT > 8                                               
220000       MOVE SPACE TO NULLPDS                                              
230000       CALL  ISPLINK USING VDEFINE N-NULLPDS    NULLPDS  CHAR             
240000                           L-NULLPDS  VDEFINE-OPT                         
250000     END-IF                                                               
260000     IF RETURN-CODE NOT > 8                                               
270000       MOVE ZERO  TO ADATE                                                
280000       CALL  ISPLINK USING VDEFINE N-ADATE      ADATE    CHAR             
290000                           L-ADATE    VDEFINE-OPT                         
300000     END-IF                                                               
310000     IF RETURN-CODE NOT > 8                                               
320000       MOVE SPACE TO MINSNR                                               
330000       CALL  ISPLINK USING VDEFINE N-MINSNR     MINSNR   CHAR             
340000                           L-MINSNR   VDEFINE-OPT                         
350000     END-IF                                                               
360000     IF RETURN-CODE NOT > 8                                               
370000       MOVE SPACE TO MAXTYP                                               
380000       CALL  ISPLINK USING VDEFINE N-MAXTYP     MAXTYP   CHAR             
390000                           L-MAXTYP   VDEFINE-OPT                         
400000     END-IF                                                               
410000     IF RETURN-CODE NOT > 8                                               
420000       MOVE SPACE TO WPROCESS                                             
430000       CALL  ISPLINK USING VDEFINE N-WPROCESS WPROCESS CHAR               
440000                           L-WPROCESS VDEFINE-OPT                         
450000     END-IF                                                               
460000     IF RETURN-CODE NOT > 8                                               
470000       MOVE SPACE TO WSTATUS                                              
480000       CALL  ISPLINK USING VDEFINE N-WSTATUS    WSTATUS  CHAR             
490000                           L-WSTATUS  VDEFINE-OPT                         
500000     END-IF                                                               
510000     IF RETURN-CODE NOT > 8                                               
520000       MOVE SPACE TO WMTYP                                                
530000       CALL  ISPLINK USING VDEFINE N-WMTYP      WMTYP    CHAR             
540000                           L-WMTYP    VDEFINE-OPT                         
550000     END-IF                                                               
560000     IF RETURN-CODE NOT > 8                                               
570000       MOVE SPACE TO XMTYP                                                
580000       CALL  ISPLINK USING VDEFINE N-XMTYP      XMTYP    CHAR             
590000                           L-XMTYP    VDEFINE-OPT                         
600000     END-IF                                                               
610000     IF RETURN-CODE NOT > 8                                               
620000       MOVE SPACE TO WSTYPE                                               
630000       CALL  ISPLINK USING VDEFINE N-WSTYPE     WSTYPE   CHAR             
640000                           L-WSTYPE   VDEFINE-OPT                         
650000     END-IF                                                               
660000     IF RETURN-CODE NOT > 8                                               
670000       MOVE SPACE TO WCATALOG                                             
680000       CALL  ISPLINK USING VDEFINE N-WCATALOG WCATALOG CHAR               
690000                           L-WCATALOG VDEFINE-OPT                         
700000     END-IF                                                               
710000     IF RETURN-CODE NOT > 8                                               
720000       MOVE ZERO  TO WLADATE                                              
730000       CALL  ISPLINK USING VDEFINE N-WLADATE    WLADATE  CHAR             
740000                           L-WLADATE  VDEFINE-OPT                         
750000     END-IF                                                               
760000     IF RETURN-CODE NOT > 8                                               
770000       MOVE ZERO  TO WLEDATE                                              
780000       CALL  ISPLINK USING VDEFINE N-WLEDATE    WLEDATE  CHAR             
790000                           L-WLEDATE  VDEFINE-OPT                         
800000     END-IF                                                               
810000     IF RETURN-CODE NOT > 8                                               
820000       MOVE ZERO  TO WLSTIME                                              
830000       CALL  ISPLINK USING VDEFINE N-WLSTIME    WLSTIME  CHAR             
840000                           L-WLSTIME  VDEFINE-OPT                         
850000     END-IF                                                               
860000     IF RETURN-CODE NOT > 8                                               
870000       MOVE ZERO  TO WLETIME                                              
880000       CALL  ISPLINK USING VDEFINE N-WLETIME    WLETIME  CHAR             
890000                           L-WLETIME  VDEFINE-OPT                         
900000     END-IF                                                               
910000     IF RETURN-CODE NOT > 8                                               
920000       MOVE ZERO  TO WLXTIME                                              
930000       CALL  ISPLINK USING VDEFINE N-WLXTIME    WLXTIME  CHAR             
940000                           L-WLXTIME  VDEFINE-OPT                         
950000     END-IF                                                               
960000     IF RETURN-CODE NOT > 8                                               
970000       MOVE ZERO  TO WMXTIME                                              
980000       CALL  ISPLINK USING VDEFINE N-WMXTIME    WMXTIME  CHAR             
990000                           L-WMXTIME  VDEFINE-OPT                         
000000     END-IF                                                               
010024     IF RETURN-CODE NOT > 8                                               
020024       MOVE ZERO  TO WODATE                                               
030024       CALL  ISPLINK USING VDEFINE N-WODATE     WODATE   CHAR             
040024                           L-WODATE   VDEFINE-OPT                         
050024     END-IF                                                               
060000     IF RETURN-CODE NOT > 8                                               
070000       MOVE SPACE TO WHOLD                                                
080000       CALL  ISPLINK USING VDEFINE N-WHOLD      WHOLD    CHAR             
090000                           L-WHOLD    VDEFINE-OPT                         
100000     END-IF                                                               
110000     IF RETURN-CODE NOT > 8                                               
120000       MOVE SPACE TO WATTN                                                
130000       CALL  ISPLINK USING VDEFINE N-WATTN      WATTN    CHAR             
140000                           L-WATTN    VDEFINE-OPT                         
150000     END-IF                                                               
160000     IF RETURN-CODE NOT > 8                                               
170000       MOVE SPACE TO WAQ                                                  
180000       CALL  ISPLINK USING VDEFINE N-WAQ        WAQ      CHAR             
190000                           L-WAQ      VDEFINE-OPT                         
200000     END-IF                                                               
210000     IF RETURN-CODE NOT > 8                                               
220000       MOVE SPACE TO WDQ                                                  
230000       CALL  ISPLINK USING VDEFINE N-WDQ        WDQ      CHAR             
240000                           L-WDQ      VDEFINE-OPT                         
250000     END-IF                                                               
260000     IF RETURN-CODE NOT > 8                                               
270000       MOVE SPACE TO WPRED                                                
280000       CALL  ISPLINK USING VDEFINE N-WPRED      WPRED    CHAR             
290000                           L-WPRED    VDEFINE-OPT                         
300000     END-IF                                                               
310000     IF RETURN-CODE NOT > 8                                               
320000       MOVE SPACE TO WSUCC                                                
330000       CALL  ISPLINK USING VDEFINE N-WSUCC      WSUCC    CHAR             
340000                           L-WSUCC    VDEFINE-OPT                         
350000     END-IF                                                               
360000     IF RETURN-CODE NOT > 8                                               
370000       MOVE SPACE TO WRES                                                 
380000       CALL  ISPLINK USING VDEFINE N-WRES       WRES     CHAR             
390000                           L-WRES     VDEFINE-OPT                         
400000     END-IF                                                               
410000     IF RETURN-CODE NOT > 8                                               
420000       MOVE SPACE TO WPARENT                                              
430000       CALL  ISPLINK USING VDEFINE N-WPARENT    WPARENT  CHAR             
440000                           L-WPARENT  VDEFINE-OPT                         
450000     END-IF                                                               
460000     IF RETURN-CODE NOT > 8                                               
470000       MOVE SPACE TO WPRIO                                                
480000       CALL  ISPLINK USING VDEFINE N-WPRIO      WPRIO    CHAR             
490000                           L-WPRIO    VDEFINE-OPT                         
500000     END-IF                                                               
510000     IF RETURN-CODE NOT > 8                                               
520000       MOVE SPACE TO WVOUT                                                
530000       CALL  ISPLINK USING VDEFINE N-WVOUT      WVOUT    CHAR             
540000                           L-WVOUT    VDEFINE-OPT                         
550000     END-IF                                                               
560000     IF RETURN-CODE NOT > 8                                               
570000       MOVE SPACE TO WSYMB                                                
580000       CALL  ISPLINK USING VDEFINE N-WSYMB      WSYMB    CHAR             
590000                           L-WSYMB    VDEFINE-OPT                         
600000     END-IF                                                               
610000     IF RETURN-CODE NOT > 8                                               
620000       MOVE SPACE TO WSYMBOL                                              
630000       CALL  ISPLINK USING VDEFINE N-WSYMBOL    WSYMBOL  CHAR             
640000                           L-WSYMBOL  VDEFINE-OPT                         
650000     END-IF                                                               
660000     IF RETURN-CODE NOT > 8                                               
670000       MOVE SPACE TO WABJOB                                               
680000       CALL  ISPLINK USING VDEFINE N-WABJOB     WABJOB   CHAR             
690000                           L-WABJOB   VDEFINE-OPT                         
700000     END-IF                                                               
710000     IF RETURN-CODE NOT > 8                                               
720000       MOVE SPACE TO WADD                                                 
730000       CALL  ISPLINK USING VDEFINE N-ADD       WADD      CHAR             
740000                           L-ADD      VDEFINE-OPT                         
750000     END-IF                                                               
760000     IF RETURN-CODE NOT > 8                                               
770000       MOVE ZERO  TO WTJCLD                                               
780000       CALL  ISPLINK USING VDEFINE N-WTJCLD     WTJCLD   FIXED            
790000                           L-WTJCLD   VDEFINE-OPT                         
800000     END-IF                                                               
810000     IF RETURN-CODE NOT > 8                                               
820000       MOVE ZERO  TO ZTDTOP                                               
830000       CALL  ISPLINK USING VDEFINE N-ZTDTOP     ZTDTOP   FIXED            
840000                           L-ZTDTOP   VDEFINE-OPT                         
850000     END-IF                                                               
860000     IF RETURN-CODE NOT > 8                                               
870000       MOVE SPACE TO TPROCESS                                             
880000       CALL  ISPLINK USING VDEFINE N-TPROCESS TPROCESS CHAR               
890000                           L-TPROCESS VDEFINE-OPT                         
900000     END-IF                                                               
910000     IF RETURN-CODE NOT > 8                                               
920000       MOVE SPACE TO TPARENT                                              
930000       CALL  ISPLINK USING VDEFINE N-TPARENT    TPARENT  CHAR             
940000                           L-TPARENT  VDEFINE-OPT                         
950000     END-IF                                                               
960000     IF RETURN-CODE NOT > 8                                               
970000       MOVE SPACE TO TSEL                                                 
980000       CALL  ISPLINK USING VDEFINE N-TSEL       TSEL     CHAR             
990000                           L-TSEL     VDEFINE-OPT                         
000000     END-IF                                                               
010000     IF RETURN-CODE NOT > 8                                               
020000       MOVE SPACE TO TMTYP                                                
030000       CALL  ISPLINK USING VDEFINE N-TMTYP      TMTYP    CHAR             
040000                           L-TMTYP    VDEFINE-OPT                         
050000     END-IF                                                               
060000     IF RETURN-CODE NOT > 8                                               
070000       MOVE SPACE TO TSTATUS                                              
080000       CALL  ISPLINK USING VDEFINE N-TSTATUS    TSTATUS  CHAR             
090000                           L-TSTATUS  VDEFINE-OPT                         
100000     END-IF                                                               
110000     IF RETURN-CODE NOT > 8                                               
120000       MOVE SPACE TO TSNR                                                 
130000       CALL  ISPLINK USING VDEFINE N-TSNR       TSNR     CHAR             
140000                           L-TSNR     VDEFINE-OPT                         
150000     END-IF                                                               
160000     IF RETURN-CODE NOT > 8                                               
170000       MOVE SPACE TO TSTYP                                                
180000       CALL  ISPLINK USING VDEFINE N-TSTYP      TSTYP    CHAR             
190000                           L-TSTYP    VDEFINE-OPT                         
200000     END-IF                                                               
210000     IF RETURN-CODE NOT > 8                                               
220000       MOVE SPACE TO TNOTE                                                
230000       CALL  ISPLINK USING VDEFINE N-TNOTE      TNOTE    CHAR             
240000                           L-TNOTE    VDEFINE-OPT                         
250000     END-IF                                                               
260000     IF RETURN-CODE NOT > 8                                               
270000       MOVE SPACE TO TATTN                                                
280000       CALL  ISPLINK USING VDEFINE N-TATTN      TATTN    CHAR             
290000                           L-TATTN    VDEFINE-OPT                         
300000     END-IF                                                               
310005     IF RETURN-CODE NOT > 8                                               
320005       MOVE SPACE TO TRES                                                 
330005       CALL  ISPLINK USING VDEFINE N-TRES       TRES     CHAR             
340005                           L-TRES     VDEFINE-OPT                         
350005     END-IF                                                               
360000     IF RETURN-CODE NOT > 8                                               
370000       MOVE ZERO  TO CDATUM                                               
380000       CALL  ISPLINK USING VDEFINE N-CDATUM     CDATUM   CHAR             
390000                           L-CDATUM   VDEFINE-OPT                         
400000     END-IF                                                               
410000     IF RETURN-CODE NOT > 8                                               
420000       MOVE SPACE TO CCATALOG                                             
430000       CALL  ISPLINK USING VDEFINE N-CCATALOG CCATALOG CHAR               
440000                           L-CCATALOG VDEFINE-OPT                         
450000     END-IF                                                               
460000     IF RETURN-CODE NOT > 8                                               
470000       MOVE SPACE TO CAQ                                                  
480000       CALL  ISPLINK USING VDEFINE N-CAQ        CAQ      CHAR             
490000                           L-CAQ      VDEFINE-OPT                         
500000     END-IF                                                               
510000     IF RETURN-CODE NOT > 8                                               
520000       MOVE SPACE TO CPQ                                                  
530000       CALL  ISPLINK USING VDEFINE N-CPQ        CPQ      CHAR             
540000                           L-CPQ      VDEFINE-OPT                         
550000     END-IF                                                               
560000     IF RETURN-CODE NOT > 8                                               
570000       MOVE SPACE TO CONF                                                 
580000       CALL  ISPLINK USING VDEFINE N-CONF       CONF     CHAR             
590000                           L-CONF     VDEFINE-OPT                         
600000     END-IF                                                               
610000     IF RETURN-CODE NOT > 8                                               
620000       MOVE ZERO  TO JTMPDUR                                              
630000       CALL  ISPLINK USING VDEFINE N-JTMPDUR    JTMPDUR  CHAR             
640000                           L-JTMPDUR  VDEFINE-OPT                         
650000     END-IF                                                               
660000     IF RETURN-CODE NOT > 8                                               
670000       MOVE SPACE TO SOPJOBST                                             
680000       CALL  ISPLINK USING VDEFINE N-SOPJOBST SOPJOBST CHAR               
690000                           L-SOPJOBST VDEFINE-OPT                         
700000     END-IF                                                               
710032     IF RETURN-CODE NOT > 8                                               
720032       MOVE SPACE TO BACKPROC                                             
730032       CALL  ISPLINK USING VDEFINE N-BACKPROC BACKPROC CHAR               
740032                           L-BACKPROC VDEFINE-OPT                         
750032     END-IF                                                               
760032     IF RETURN-CODE NOT > 8                                               
770034       MOVE SPACE TO BACKPSB                                              
780032       CALL  ISPLINK USING VDEFINE N-BACKPSB  BACKPSB  CHAR               
790032                           L-BACKPSB  VDEFINE-OPT                         
800032     END-IF                                                               
810032     IF RETURN-CODE NOT > 8                                               
820033       MOVE SPACE TO ZZSESSID                                             
830033       CALL  ISPLINK USING VDEFINE N-ZZSESSID ZZSESSID CHAR               
840033                           L-ZZSESSID VDEFINE-OPT                         
850032     END-IF                                                               
860033     IF RETURN-CODE NOT > 8                                               
870033       MOVE ZERO  TO TODATE                                               
880033       CALL  ISPLINK USING VDEFINE N-TODATE   TODATE   CHAR               
890033                           L-TODATE   VDEFINE-OPT                         
900033     END-IF                                                               
910032     IF RETURN-CODE NOT > 8                                               
920032       MOVE SPACE TO TOSTATUS                                             
930032       CALL  ISPLINK USING VDEFINE N-TOSTATUS   TOSTATUS CHAR             
940032                           L-TOSTATUS VDEFINE-OPT                         
950032     END-IF                                                               
960032     IF RETURN-CODE NOT > 8                                               
970032       MOVE ZERO  TO TANR                                                 
980032       CALL  ISPLINK USING VDEFINE N-TANR     TANR     CHAR               
990032                           L-TANR     VDEFINE-OPT                         
000024     END-IF                                                               
010000     .                                                                    
020000     EJECT                                                                
030000 B-ALLOCKERINGAR SECTION.                                                 
040000     SKIP2                                                                
050000     IF SDSN NOT = SPAR-SDSN                                              
060000        IF SOP-OPPNAD = JA                                                
070000          MOVE '9'   TO SOP20-KDSOPFUNK                                   
080000          PERFORM S2-CALL-SOP20                                           
090000          MOVE NEJ TO SOP-OPPNAD                                          
100000        END-IF                                                            
110000        MOVE SOPDDN1  TO ALLOC-DDN                                        
120000        MOVE SDSN TO SPAR-SDSN ALLOC-DSN                                  
130000        PERFORM BA-ALLOC-DS                                               
140000        MOVE '0'   TO SOP20-KDSOPFUNK                                     
150000        PERFORM S2-CALL-SOP20                                             
160000        MOVE JA TO SOP-OPPNAD                                             
170000                                                                          
180025        MOVE SPACE TO DB-STATUS-VAERDE                                    
190001        CALL W980WSPC USING FGETF WSRKOD  SOP-PARM                        
200000                      DB-STATUS-PARM  DB-STATUS-VAERDE-PARM               
210001        CALL W980WSPC USING FQUIT WSRKOD                                  
220000     END-IF                                                               
230000     IF JDSN NOT = SPAR-JDSN                                              
240000        MOVE PDSDDN1  TO ALLOC-DDN                                        
250000        MOVE JDSN TO SPAR-JDSN ALLOC-DSN                                  
260000        PERFORM BA-ALLOC-DS                                               
270024        MOVE JA TO JCL-ALLOCATED                                          
280000     END-IF                                                               
290000     IF JDSN2 NOT = SPAR-JDSN2                                            
300000        MOVE PDSDDN2  TO ALLOC-DDN                                        
310000        MOVE JDSN2 TO SPAR-JDSN2 ALLOC-DSN                                
320000        PERFORM BA-ALLOC-DS                                               
330000        MOVE JA TO JCL-ALLOCATED                                          
340000     END-IF                                                               
350000     .                                                                    
360000     EJECT                                                                
370000 BA-ALLOC-DS   SECTION.                                                   
380000     SKIP2                                                                
390000     CALL ISPLINK USING ISP-SELECT L-ALLOC-CMD    ALLOC-CMD               
400000     IF RETURN-CODE > 4                                                   
410000        MOVE ALLOC-DSN  TO XDSN                                           
420000        CALL ISPLINK USING SETMSG ERROR-ALLOC-FAIL                        
430000        MOVE ERROR-ALLOC-FAIL TO ZERRMSG                                  
440000     END-IF                                                               
450000     .                                                                    
460000     EJECT                                                                
470000 C-SOP20 SECTION.                                                         
480000     SKIP2                                                                
490000     MOVE FUNC TO TSEL                                                    
500000     MOVE SPROCESS TO TPROCESS                                            
510024     MOVE SPACE TO WS-SEL                                                 
520000     PERFORM S1-SOP20                                                     
530000     .                                                                    
540000     EJECT                                                                
550000 D-EXPAND-PROCESS SECTION.                                                
560000     SKIP2                                                                
570000     MOVE 1 TO HIX                                                        
580000     MOVE SPROCESS TO H-IDPROCESS (1)                                     
590000                                                                          
600000     PERFORM S15-SKAPA-MSL-TABELL                                         
601041     IF HIX > 0                                                           
610041       PERFORM S10-DISPLAY-TABELL                                         
611041     END-IF                                                               
620000     .                                                                    
630000     SKIP3                                                                
640000 E-LIST-STARTED SECTION.                                                  
650000     SKIP2                                                                
660000     MOVE 1 TO HIX                                                        
670000     MOVE SPACE TO H-IDPROCESS (1)                                        
680000                                                                          
690000     PERFORM S16-SKAPA-STARTED-MSL-TABELL                                 
700027     IF HIX > 0                                                           
710027       PERFORM S10-DISPLAY-TABELL                                         
720027     END-IF                                                               
730000     .                                                                    
740000     EJECT                                                                
750000 F-DISPLAY-KALENDER SECTION.                                              
760000     SKIP2                                                                
770000     MOVE FUNC TO SOP20-KDSOPFUNK                                         
780000     MOVE SPACE TO SOP20-IDPROCESS                                        
790000     MOVE ADATE TO SOP20-TIAPDAT                                          
800000     MOVE SPACE TO SOP20-TECATALOG                                        
810000     PERFORM S2-CALL-SOP20                                                
820000     IF SOP20-KDRET = 0                                                   
830000       MOVE SOP20-TIAPDAT TO CDATUM                                       
840000       MOVE SOP20-TECATALOG TO CCATALOG                                   
850000       MOVE SOP20-TEACTQ TO CAQ                                           
860000       MOVE SOP20-TEPASSQ TO CPQ                                          
870000       CALL ISPLINK USING ISP-DISPLAY KALENDER-PANEL                      
880000     END-IF                                                               
890000     .                                                                    
900000     EJECT                                                                
910000 G-TEMPANDRING-JCL  SECTION.                                              
920000     SKIP2                                                                
930000     MOVE FUNC TO TSEL                                                    
940000     MOVE SPROCESS TO TPROCESS                                            
950000     PERFORM S3-TEMPANDRING-JCL                                           
960000     .                                                                    
970000     EJECT                                                                
980000 H-LAGRA-SYMBOLER  SECTION.                                               
990000     SKIP2                                                                
000000     MOVE FUNC TO SOP20-KDSOPFUNK                                         
010000     MOVE SPROCESS TO TPROCESS                                            
020000     PERFORM S8-LAGRA-SYMBOLER                                            
030000     .                                                                    
040000     EJECT                                                                
050000 I-DISPLAY-SDSF   SECTION.                                                
060000     SKIP2                                                                
070000     MOVE SPROCESS TO TPROCESS                                            
080000     PERFORM S6-DISPLAY-SDSF                                              
090000     .                                                                    
100000     EJECT                                                                
110024 J-DISPLAY-ORDER-QUEUE SECTION.                                           
120024     SKIP2                                                                
130024     MOVE SPROCESS TO TPROCESS                                            
140024     PERFORM S6-DISPLAY-ORDER-QUEUE                                       
150024     .                                                                    
160024     EJECT                                                                
170032 K-ACTIVATE-BACKOUT SECTION.                                              
180032     SKIP2                                                                
190035     MOVE SPROCESS TO TPROCESS                                            
200035     PERFORM S9-ACTIVATE-BACKOUT                                          
210035     .                                                                    
220032     EJECT                                                                
230000 S1-SOP20   SECTION.                                                      
240000     SKIP2                                                                
250030     IF TSEL = 'S' OR 'E' OR 'A' OR 'P' OR 'O' OR 'C' OR 'D'              
260000       IF TSEL = 'O'                                                      
270000         MOVE 'ORDER' TO CONF                                             
280024       ELSE IF TSEL = 'C' OR 'D'                                          
290000         MOVE 'CANCEL' TO CONF                                            
300030       ELSE IF TSEL = 'S'                                                 
310024         MOVE 'START' TO CONF                                             
320000       ELSE IF TSEL = 'E'                                                 
330000         MOVE 'END' TO CONF                                               
340000       ELSE IF TSEL = 'A'                                                 
350000         MOVE 'ACTIVATE' TO CONF                                          
360000       ELSE IF TSEL = 'P'                                                 
370000         MOVE 'PASSIVATE' TO CONF                                         
380000       END-IF                                                             
390000       END-IF                                                             
400000       END-IF                                                             
410000       END-IF                                                             
420000       END-IF                                                             
430000       END-IF                                                             
440000       MOVE ZERO TO RETURN-CODE                                           
450000       IF WADD = 'YES'                                                    
460000         IF TSEL = 'A' OR 'O'                                             
470000           PERFORM S1A-ADDERA-SYMBOLER                                    
480000         END-IF                                                           
490000       ELSE                                                               
500000         MOVE SPACE TO SOP20-TESYMBV                                      
510000       END-IF                                                             
520000       IF RETURN-CODE = ZERO                                              
530028         CALL ISPLINK USING ADDPOP                                        
540000         CALL ISPLINK USING ISP-DISPLAY CONFIRM-PANEL                     
550029         MOVE RETURN-CODE TO SAVE-RC                                      
560028         CALL ISPLINK USING REMPOP                                        
570029         MOVE SAVE-RC TO RETURN-CODE                                      
580000       END-IF                                                             
590000     ELSE                                                                 
600000       MOVE ZERO TO RETURN-CODE                                           
610000     END-IF                                                               
620000     IF RETURN-CODE = ZERO                                                
630000       MOVE TSEL TO SOP20-KDSOPFUNK                                       
640000       MOVE TPROCESS TO SOP20-IDPROCESS                                   
650000       MOVE ZERO TO SOP20-TIAPDAT                                         
660000       IF TSEL = 'O' OR 'C'                                               
670000         MOVE ADATE TO SOP20-TIAPDAT                                      
680000         MOVE SPACE TO SOP20-TECATALOG                                    
690000       END-IF                                                             
700024       IF TSEL = 'D'                                                      
710024         MOVE TODATE TO SOP20-TIAPDAT                                     
720024       END-IF                                                             
730024       IF TSEL = 'D' OR 'Y'                                               
740024         MOVE TANR TO SOP20-KDANR                                         
750024       END-IF                                                             
760000       PERFORM S2-CALL-SOP20                                              
770000       IF SOP20-KDSOPFUNK = 'I' AND SOP20-KDRET = 0                       
780024       AND WS-SEL = SPACE                                                 
790000         PERFORM S5-DISPLAY-INFO                                          
800000       END-IF                                                             
810024       IF SOP20-KDSOPFUNK = 'Y' AND SOP20-KDRET = 0                       
820024         PERFORM S7-DISPLAY-INFO-ANR                                      
830024       END-IF                                                             
840000     END-IF                                                               
850000     .                                                                    
860000     EJECT                                                                
870000 S1A-ADDERA-SYMBOLER   SECTION.                                           
880000     SKIP2                                                                
890000     MOVE SPROCESS TO WPROCESS                                            
900000     CALL ISPLINK USING ISP-DISPLAY SYMB-PANEL2                           
910000     MOVE SPACE TO ZERRMSG                                                
920000     MOVE SPACE TO SOP20-TESYMBV                                          
930000     IF RETURN-CODE = ZERO                                                
940000       MOVE WSYMBOL TO SOP20-TESYMBV                                      
950000     END-IF                                                               
960000     MOVE SPACE TO ZERRMSG                                                
970000     .                                                                    
980000     EJECT                                                                
990000 S2-CALL-SOP20 SECTION.                                                   
000000     SKIP2                                                                
010000     CALL W9802000 USING SOP20-W98020                                     
020000     IF SOP20-KDRET > 4                                                   
030000        MOVE SOP20-KDMEDD TO XMSGCODE                                     
040000        CALL ISPLINK USING SETMSG ERROR-FUNC-FAIL                         
050000        MOVE ERROR-FUNC-FAIL TO ZERRMSG                                   
060000     END-IF                                                               
070000     .                                                                    
080000     EJECT                                                                
090000 S3-TEMPANDRING-JCL  SECTION.                                             
100000     SKIP2                                                                
110000     MOVE TPROCESS TO P-IDPROCESS TEMP-HAMTA-IDPROCESS                    
120000                     TEMP-LAGRA-IDPROCESS                                 
130000     MOVE ZERO TO P-LENGD                                                 
140000     INSPECT P-IDPROCESS TALLYING P-LENGD                                 
150000            FOR CHARACTERS BEFORE INITIAL SPACE                           
160000     ADD 2 TO P-LENGD                                                     
170000                                                                          
180000     MOVE SPACE TO DATA-VAERDE                                            
190001     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
200000         INFO-NAMN-PARM DATA-PARM                                         
210000     IF WSRKOD NOT = SPACE                                                
220000*---   PROCESS FINNS EJ                                                   
230001       CALL W980WSPC USING FQUIT WSRKOD                                   
240000       CALL ISPLINK USING SETMSG ERROR-PROC-NOT-FOUND                     
250000       MOVE ERROR-PROC-NOT-FOUND TO ZERRMSG                               
260000       GO TO ADMIT                                                        
270000     END-IF                                                               
280000                                                                          
290000     MOVE SPACE TO DATA-VAERDE                                            
300001     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
310000         STARTTYP-PARM DATA-PARM                                          
320000                                                                          
330000     MOVE ZERO TO DATUM-PACKAT                                            
340001     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
350000         TEMP-VARAKTIGHET-PARM  DATUM-PARM                                
360000                                                                          
370001     CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                       
380000         NUDAT-NAMN-PARM NUDAT-PARM                                       
390000                                                                          
400001     CALL W980WSPC USING FQUIT WSRKOD                                     
410000                                                                          
420000     IF DATA-VAERDE NOT = 'FSUB' AND 'SUB'                                
430000*---   INKOMPATIBEL STARTTYP                                              
440000       CALL ISPLINK USING SETMSG ERROR-TEMP-NOT-ALLOWED                   
450000       MOVE ERROR-TEMP-NOT-ALLOWED TO ZERRMSG                             
460000       GO TO ADMIT                                                        
470000     END-IF                                                               
480000                                                                          
490000     MOVE DATA-VAERDE TO WSTYPE                                           
500000     MOVE DATUM-PACKAT TO JTMPDUR                                         
510000     MOVE PDSDDN2  TO TEMP-HAMTA-DDNAMN                                   
520000     MOVE PDSDDN1  TO TEMP-LAGRA-DDNAMN                                   
530000     MOVE JDSN2 TO XDSN                                                   
540000                                                                          
540140     MOVE DATUM-PACKAT   TO TMP1-YYMMDD                                   
540240     MOVE NUDAT          TO TMP2-YYMMDD                                   
540340     PERFORM WY2000P1                                                     
550040     IF DATUM-PACKAT > 0 AND                                              
560040       (DATUM-PACKAT <= 99 OR TMP1-YYMMDD >= TMP2-YYMMDD)                 
570000*---   TEMPÄNDRING FINNS REDAN                                            
580000       IF DATA-VAERDE = 'FSUB' OR 'SUB'                                   
590000         MOVE PDSDDN1  TO TEMP-HAMTA-DDNAMN                               
600000         MOVE JDSN  TO XDSN                                               
610000       END-IF                                                             
620000                                                                          
630000       CALL ISPLINK USING ISP-DISPLAY TEMP-PANEL-1                        
640000       IF RETURN-CODE > 0                                                 
650000         GO TO ADMIT                                                      
660000       ELSE                                                               
670000         IF ZCMD = 'C' OR 'CAN' OR 'CANCEL'                               
680001           CALL W980WSPC USING FDELK WSRKOD P-PARM                        
690000                 TEMP-VARAKTIGHET-PARM                                    
700001           CALL W980WSPC USING FSAVE WSRKOD                               
710000           CALL ISPLINK USING SETMSG MSG-TEMP-DELETED                     
720000           MOVE SPACE TO ZCMD                                             
730000           GO TO ADMIT                                                    
740000         END-IF                                                           
750000       END-IF                                                             
760000     END-IF                                                               
770000                                                                          
780000     MOVE XDSN TO TEMP-HAMTA-DSNAME                                       
790000     CALL ISPLINK USING ISP-SELECT L-TEMP-HAMTA-CMD                       
800000         TEMP-HAMTA-CMD                                                   
810000     IF RETURN-CODE > 4                                                   
820000       CALL ISPLINK USING SETMSG ERROR-RETRIEVE-FAIL                      
830000       MOVE ERROR-RETRIEVE-FAIL TO ZERRMSG                                
840000       GO TO ADMIT                                                        
850000     END-IF                                                               
860000                                                                          
870000     CALL ISPLINK USING ISP-EDIT TMPDSN                                   
880000                                                                          
881042     MOVE DATUM-PACKAT   TO TMP1-YYMMDD                                   
882042     MOVE NUDAT          TO TMP2-YYMMDD                                   
883043     PERFORM WY2000P1                                                     
890042     IF RETURN-CODE = 0                                                   
890142     OR (DATUM-PACKAT > 0                                                 
891042         AND (DATUM-PACKAT <= 99 OR TMP1-YYMMDD >= TMP2-YYMMDD))          
900042*---     SAVE GJORD,  ELLER TIDIGARE GÄLLANDE TEMPÄNDRING                 
910000       IF RETURN-CODE = 0                                                 
920000         IF (DATA-VAERDE = 'FSUB' OR 'SUB')                               
930000            MOVE JDSN  TO XDSN                                            
940000         END-IF                                                           
950000         MOVE XDSN TO TEMP-LAGRA-DSNAME                                   
960000         CALL ISPLINK USING ISP-SELECT L-TEMP-LAGRA-CMD                   
970000              TEMP-LAGRA-CMD                                              
980000         IF RETURN-CODE > 4                                               
990000            CALL ISPLINK USING SETMSG ERROR-STORE-FAIL                    
000000            MOVE ERROR-STORE-FAIL TO ZERRMSG                              
010000            GO TO ADMIT                                                   
020000         END-IF                                                           
030000       END-IF                                                             
040000                                                                          
050000       CALL ISPLINK USING ISP-DISPLAY TEMP-PANEL-2                        
060000       IF RETURN-CODE = 0                                                 
070000                                                                          
080000         MOVE JTMPDUR TO DATUM-PACKAT                                     
090001         CALL W980WSPC USING FSET WSRKOD P-PARM                           
100000              TEMP-VARAKTIGHET-PARM DATUM-PARM                            
110001         CALL W980WSPC USING FSAVE WSRKOD                                 
120000         CALL ISPLINK USING SETMSG MSG-TEMP-ACTIVATED                     
130000                                                                          
140000       END-IF                                                             
150000     END-IF                                                               
160000     .                                                                    
170000 ADMIT.                                                                   
180000       CONTINUE                                                           
190000     .                                                                    
200000     EJECT                                                                
210000 S5-DISPLAY-INFO    SECTION.                                              
220000     SKIP2                                                                
230000     MOVE SOP20-IDPROCESS TO WPROCESS                                     
240000     MOVE SOP20-KDPROCMTYP TO WMTYP                                       
250000     MOVE SOP20-TECATALOG TO WCATALOG                                     
260000     MOVE SOP20-KDPROCSTRT TO WSTYPE                                      
270000     MOVE SOP20-KDPROCSTAT TO WSTATUS                                     
280000     MOVE SOP20-FLHOLD TO WHOLD                                           
290000     MOVE SOP20-TEATTN TO WATTN                                           
300000     MOVE SOP20-TEPRED TO WPRED                                           
310000     MOVE SOP20-TESUCC TO WSUCC                                           
320000     MOVE SOP20-TERES TO WRES                                             
330000     MOVE SOP20-TIAPDAT-SENAST TO WLADATE                                 
340000     MOVE SOP20-TIEXDAT-SENAST TO WLEDATE                                 
350000     MOVE SOP20-TIMINUT-START TO WLSTIME                                  
360000     MOVE SOP20-TIMINUT-STOPP TO WLETIME                                  
370000     MOVE SOP20-TIEXEC-SENAST TO WLXTIME                                  
380000     MOVE SOP20-TIEXEC-MEDEL TO WMXTIME                                   
390000     MOVE SOP20-TEACTQ TO WAQ                                             
400000     MOVE SOP20-TEPASSQ TO WDQ                                            
410000     MOVE SOP20-IDPROCESS-PARENT TO WPARENT                               
420000     MOVE SOP20-KDPROCPRIO       TO WPRIO                                 
430000     MOVE SOP20-KDVOUT           TO WVOUT                                 
440000     MOVE SOP20-TITMPDUR         TO WTJCLD                                
450000     MOVE SOP20-TESYMBV          TO WSYMB                                 
460000     MOVE SOP20-ID-ABEND-JOB TO WABJOB                                    
470000     IF SOP20-KDPROCMTYP = 'J'                                            
480000       PERFORM S46-INFO-JOB-STATUS                                        
490000     END-IF                                                               
500000                                                                          
510000     CALL ISPLINK USING ISP-DISPLAY INFO-PANEL                            
520000     .                                                                    
530000     EJECT                                                                
540000 S6-DISPLAY-SDSF   SECTION.                                               
550000     SKIP2                                                                
560000     MOVE TPROCESS TO P-IDPROCESS XPROCESS                                
570000     MOVE SPACE TO DATA-VAERDE                                            
580000     MOVE +0 TO P-LENGD                                                   
590000     INSPECT P-IDPROCESS TALLYING P-LENGD                                 
600000        FOR CHARACTERS BEFORE INITIAL SPACE                               
610000     ADD +2 TO P-LENGD                                                    
620001     CALL W980WSPC USING FGETF WSRKOD P-PARM MTYP-PARM                    
630000                DATA-PARM                                                 
640000     IF WSRKOD = SPACE                                                    
650001       CALL W980WSPC USING FQUIT WSRKOD                                   
660000       IF DATA-VAERDE = 'S' OR 'P'                                        
670000         CALL ISPLINK USING SETMSG NO-SYS-HOLD-Q                          
680000         MOVE NO-SYS-HOLD-Q  TO ZERRMSG                                   
690000       ELSE                                                               
700000         IF DATA-VAERDE = 'R' OR 'J'                                      
710000           MOVE DATA-VAERDE TO XMTYP                                      
720000           MOVE SPACE TO DATA-VAERDE                                      
730001           CALL W980WSPC USING FGETF WSRKOD SOP-PARM                      
740000                DB-STATUS-PARM DATA-PARM                                  
750000           IF WSRKOD = SPACE AND DATA-VAERDE = 'TEST'                     
760000             MOVE P-IDPROCESS TO JOBB-NAMN                                
770000             MOVE 'T' TO JOBB-NAMN-5                                      
780000             MOVE JOBB-NAMN  TO XPROCESS                                  
790000           END-IF                                                         
800001           CALL W980WSPC USING FQUIT WSRKOD                               
810025           CALL ISPLINK USING VPUT N-XPROCESS ISP-SHARED                  
820025           CALL ISPLINK USING VPUT N-XMTYP ISP-SHARED                     
830000           CALL ISPLINK USING ISP-SELECT L-DISPLAY-SDSF-CMD               
840000                     DISPLAY-SDSF-CMD                                     
850000         ELSE                                                             
860000           CALL ISPLINK USING SETMSG ERROR-ILLEGAL-MTYP                   
870000           MOVE ERROR-ILLEGAL-MTYP  TO ZERRMSG                            
880000         END-IF                                                           
890000       END-IF                                                             
900000     ELSE                                                                 
910001       CALL W980WSPC USING FQUIT WSRKOD                                   
920000       CALL ISPLINK USING SETMSG ERROR-PROC-NOT-FOUND                     
930000       MOVE ERROR-PROC-NOT-FOUND TO ZERRMSG                               
940000     END-IF                                                               
950000     .                                                                    
960000     EJECT                                                                
970024 S6-DISPLAY-ORDER-QUEUE SECTION.                                          
980024     SKIP2                                                                
990024     MOVE TPROCESS TO P-IDPROCESS SOP20-IDPROCESS                         
000024                                                                          
010024     MOVE 'I' TO SOP20-KDSOPFUNK                                          
020024                                                                          
030024     CALL W9802000 USING SOP20-W98020                                     
040024                                                                          
050024     IF SOP20-KDRET > 4                                                   
060024*---   PROCESS FINNS EJ                                                   
070024        CALL ISPLINK USING SETMSG ERROR-FUNC-FAIL                         
080024        MOVE ERROR-FUNC-FAIL TO ZERRMSG                                   
090024     ELSE                                                                 
100024       PERFORM S17-SKAPA-ORDER-QUEUE-TABELL                               
110024       PERFORM S11-DISPLAY-OQ-TABELL                                      
120026       MOVE 'OQ' TO TSEL                                                  
130024     END-IF                                                               
140024     .                                                                    
150024     EJECT                                                                
160024 S7-DISPLAY-INFO-ANR    SECTION.                                          
170024     SKIP2                                                                
180024     MOVE SOP20-IDPROCESS TO WPROCESS                                     
190024     MOVE SOP20-TESYMBV   TO WSYMB                                        
200024                                                                          
210024     CALL ISPLINK USING ISP-DISPLAY INFO2-PANEL                           
220024     .                                                                    
230024     EJECT                                                                
240000 S8-LAGRA-SYMBOLER  SECTION.                                              
250000     SKIP2                                                                
260000     MOVE FUNC TO SOP20-KDSOPFUNK                                         
270000     MOVE TPROCESS TO WPROCESS P-IDPROCESS SOP20-IDPROCESS                
280000     MOVE ZERO TO P-LENGD                                                 
290000     INSPECT P-IDPROCESS TALLYING P-LENGD                                 
300000            FOR CHARACTERS BEFORE INITIAL SPACE                           
310000     ADD 2 TO P-LENGD                                                     
320000                                                                          
330000     MOVE 'I' TO SOP20-KDSOPFUNK                                          
340000                                                                          
350000     CALL W9802000 USING SOP20-W98020                                     
360000                                                                          
370000     IF SOP20-KDRET > 4                                                   
380000*---   PROCESS FINNS EJ                                                   
390000        CALL ISPLINK USING SETMSG ERROR-FUNC-FAIL                         
400000        MOVE ERROR-FUNC-FAIL TO ZERRMSG                                   
410000     ELSE                                                                 
420000       MOVE 'V' TO SOP20-KDSOPFUNK                                        
430000       MOVE SOP20-TESYMBV TO WSYMBOL                                      
440000       CALL ISPLINK USING ISP-DISPLAY SYMB-PANEL ZERRMSG                  
450000       PERFORM UNTIL RETURN-CODE > 0                                      
460000         MOVE SPACE TO ZERRMSG                                            
470000         IF WSYMBOL NOT = SPACE                                           
480000           MOVE SPACE TO SOP20-TESYMBV                                    
490000           MOVE WSYMBOL TO SOP20-TESYMBV                                  
500000           CALL W9802000 USING SOP20-W98020                               
510000         END-IF                                                           
520000         CALL ISPLINK USING ISP-DISPLAY SYMB-PANEL ZERRMSG                
530000       END-PERFORM                                                        
540000       MOVE SPACE TO ZERRMSG                                              
550000     END-IF                                                               
560000     .                                                                    
570035     EJECT                                                                
580035 S9-ACTIVATE-BACKOUT SECTION.                                             
590035     SKIP2                                                                
591044     IF TPROCESS(6:1) = '0'                                               
592044       MOVE '0' TO W-BACKPROC-POS6                                        
593044     ELSE                                                                 
593144       MOVE '?' TO W-BACKPROC-POS6                                        
594044     END-IF                                                               
600035     STRING TPROCESS (1:4) DELIMITED BY SIZE                              
610035            'P'            DELIMITED BY SIZE                              
620044            W-BACKPROC-POS6 DELIMITED BY SIZE                             
621044            TPROCESS (7:2) DELIMITED BY SIZE                              
630035                    INTO BACKPROC                                         
640035     STRING TPROCESS (1:4) DELIMITED BY SIZE                              
650035            '??'           DELIMITED BY SIZE                              
660035                    INTO BACKPSB                                          
670035                                                                          
680035     CALL ISPLINK USING ADDPOP                                            
690035     CALL ISPLINK USING ISP-DISPLAY BACKOUT-PANEL                         
700035     MOVE RETURN-CODE TO SAVE-RC                                          
710035     CALL ISPLINK USING REMPOP                                            
720035     MOVE SAVE-RC TO RETURN-CODE                                          
730035                                                                          
740035     IF RETURN-CODE = ZERO                                                
750035       MOVE 'A'  TO SOP20-KDSOPFUNK                                       
760039       MOVE 'WBACKOUT'  TO SOP20-IDPROCESS                                
770035       MOVE ZERO TO SOP20-TIAPDAT                                         
780035       MOVE SPACE TO SOP20-TECATALOG                                      
790035       MOVE SPACE TO SOP20-TESYMBV                                        
800035                                                                          
810039       STRING TPROCESS       DELIMITED BY SPACE                           
831038              ')'            DELIMITED BY SIZE                            
840035                    INTO SYMB-BACKJOB                                     
850035       MOVE SPACE    TO  SYMB-BACKUSER                                    
860035       STRING ZZSESSID DELIMITED  BY SPACE                                
870035              ')'      DELIMITED BY SIZE                                  
880035                    INTO SYMB-BACKUSER                                    
890035       MOVE SPACE    TO  SYMB-BACKPROC                                    
900035       STRING BACKPROC DELIMITED  BY SPACE                                
910035              ')'      DELIMITED BY SIZE                                  
920035                    INTO SYMB-BACKPROC                                    
930035       MOVE SPACE    TO  SYMB-BACKPSB                                     
940035       STRING BACKPSB  DELIMITED  BY SPACE                                
950035              ')'      DELIMITED BY SIZE                                  
960035                    INTO SYMB-BACKPSB                                     
970035                                                                          
980035       MOVE BACKOUT-TESYMBV TO SOP20-TESYMBV                              
990035       PERFORM S2-CALL-SOP20                                              
000035     END-IF                                                               
010035     .                                                                    
020000     EJECT                                                                
030000 S10-DISPLAY-TABELL SECTION.                                              
040000     SKIP2                                                                
050000     MOVE H-IDPROCESS (HIX) TO TPARENT                                    
060000     MOVE HIX TO TB-NR                                                    
070000     MOVE ZERO TO H-TBDISPL-RC (HIX)                                      
080000     MOVE NEJ TO AUTO-SWITCH                                              
090000                                                                          
100027     PERFORM UNTIL HIX <= 0 OR HIX >= HIX-FULL                            
110000                                                                          
120000       IF AUTO-SWITCH = JA                                                
130001         CALL W009WAIT USING AUTO-PERIOD                                  
140000         CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY ISP-LOCK              
150000       END-IF                                                             
160000                                                                          
170000       IF H-TBDISPL-RC (HIX) = 4                                          
180000       OR (ZERRMSG NOT = SPACE                                            
190000                     AND ERROR-NO-STARTED                                 
200000                     AND ERROR-NO-SUIT-TYPE                               
210000                     AND MSG-AUTO-INTERRUPTED)                            
220000         CALL ISPLINK USING TBDISPL TB-NAMN NOPARM ZERRMSG                
230000               N-TSEL CRP    NOPARM NOPARM N-CRP                          
240000       ELSE                                                               
250000         CALL ISPLINK USING TBTOP TB-NAMN                                 
260000         CALL ISPLINK USING TBSKIP TB-NAMN ZTDTOP                         
270000         CALL ISPLINK USING TBDISPL TB-NAMN MSL-PANEL ZERRMSG             
280000               NOPARM NOPARM NOPARM NOPARM N-CRP                          
290000       END-IF                                                             
300000       MOVE RETURN-CODE TO H-TBDISPL-RC (HIX)                             
310000       MOVE SPACE TO ZERRMSG                                              
320000                                                                          
330000       IF  RETURN-CODE < 8                                                
340000         IF  TSEL NOT = SPACE                                             
350000                                                                          
360000           CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY SAVE                
370000           IF TSEL = 'X'                                                  
380000             ADD 1 TO HIX                                                 
390000             MOVE TPROCESS TO H-IDPROCESS (HIX) TPARENT                   
400000             PERFORM S15-SKAPA-MSL-TABELL                                 
410000           ELSE IF TSEL = 'J'                                             
420000             PERFORM S3-TEMPANDRING-JCL                                   
430000             PERFORM S25-UPPDAT-TAB-RAD                                   
440000             PERFORM S45-ETT-JOB-STATUS                                   
450001             CALL W980WSPC USING FSAVE WSRKOD                             
460000             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
470000           ELSE IF TSEL = 'Q'                                             
480000             PERFORM S6-DISPLAY-SDSF                                      
490000             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
500024           ELSE IF TSEL = 'OQ'                                            
510024             PERFORM S6-DISPLAY-ORDER-QUEUE                               
520024             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
530000           ELSE IF TSEL = 'V'                                             
540000             PERFORM S8-LAGRA-SYMBOLER                                    
550000             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
560035           ELSE IF TSEL = 'B'                                             
570037             PERFORM S9-ACTIVATE-BACKOUT                                  
580035             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
590000           ELSE                                                           
600024             MOVE SPACE TO WS-SEL                                         
610000             PERFORM S1-SOP20                                             
620000             PERFORM S25-UPPDAT-TAB-RAD                                   
630000             PERFORM S45-ETT-JOB-STATUS                                   
640001             CALL W980WSPC USING FSAVE WSRKOD                             
650000             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
660000           END-IF                                                         
661035           END-IF                                                         
670024           END-IF                                                         
680000           END-IF                                                         
690000           END-IF                                                         
700000           END-IF                                                         
710000           IF ZERRMSG = SPACE                                             
720000             MOVE SPACE TO TSEL                                           
730000             CALL ISPLINK USING TBPUT TB-NAMN                             
740000           END-IF                                                         
750000                                                                          
760000         ELSE IF TSEL = SPACE                                             
770000                                                                          
780000           IF ZCMD NOT = SPACE                                            
790000             UNSTRING ZCMD DELIMITED BY ALL SPACE                         
800000             INTO ORD1 ORD2                                               
810000             IF ORD1 = 'AUTO'                                             
820000               UNSTRING ORD2 DELIMITED BY SPACE INTO NNX                  
830000               INSPECT NNX REPLACING LEADING SPACE BY ZERO                
840000               COMPUTE AUTO-PERIOD = NN * 6000                            
850000               MOVE JA TO AUTO-SWITCH                                     
860000             ELSE                                                         
870000               CALL ISPLINK USING SETMSG ERROR-ILLEGAL-CMD                
880000               MOVE ERROR-ILLEGAL-CMD TO ZERRMSG                          
890000             END-IF                                                       
900000           END-IF                                                         
910000                                                                          
920000           IF HIX = 1 AND FUNC = 'LS' AND H-TBDISPL-RC (1) = 0            
930000             CALL ISPLINK USING TBEND TB-NAMN                             
940000             PERFORM S16-SKAPA-STARTED-MSL-TABELL                         
950000           ELSE                                                           
960000             PERFORM S20-UPPDATERA-MSL-TABELL                             
970001             CALL W980WSPC USING FSAVE WSRKOD                             
980000             PERFORM S40-UPPDATERA-JOB-STATUS                             
990000           END-IF                                                         
000000         END-IF                                                           
010000         END-IF                                                           
020000                                                                          
030000       ELSE                                                               
040000                                                                          
050000         CALL ISPLINK USING TBEND TB-NAMN                                 
060000         MOVE H-PARM (HIX) TO P-PARM                                      
070000         SUBTRACT 1 FROM HIX                                              
080000         IF HIX > 0                                                       
090000           CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE             
100000           MOVE H-IDPROCESS (HIX) TO TPARENT                              
110000           MOVE HIX TO TB-NR                                              
120000           MOVE P-IDPROCESS TO TPROCESS                                   
130000           PERFORM S25-UPPDAT-TAB-RAD                                     
140000           PERFORM S45-ETT-JOB-STATUS                                     
150001           CALL W980WSPC USING FSAVE WSRKOD                               
160000         END-IF                                                           
170000                                                                          
180000       END-IF                                                             
190000                                                                          
200000     END-PERFORM                                                          
210000     .                                                                    
220000     EJECT                                                                
230024 S11-DISPLAY-OQ-TABELL SECTION.                                           
240024     SKIP2                                                                
250024                                                                          
260024     MOVE ZERO TO TBOQDISPL-RC                                            
270024     PERFORM UNTIL  TBOQDISPL-RC > 4                                      
280024       OR ZERRMSG = ERROR-NO-ORDER-QUEUE                                  
290024                                                                          
300024       IF TBOQDISPL-RC = 4                                                
310024       OR ZERRMSG NOT = SPACE                                             
320024         CALL ISPLINK USING TBDISPL TBOQ-NAMN NOPARM ZERRMSG              
330024               N-TSEL CRP    NOPARM NOPARM N-CRP                          
340024       ELSE                                                               
350024         CALL ISPLINK USING TBTOP TBOQ-NAMN                               
360024         CALL ISPLINK USING TBSKIP TBOQ-NAMN ZTDTOP                       
370024         CALL ISPLINK USING TBDISPL TBOQ-NAMN OQ-PANEL ZERRMSG            
380024               NOPARM NOPARM NOPARM NOPARM N-CRP                          
390024       END-IF                                                             
400024       MOVE RETURN-CODE TO TBOQDISPL-RC                                   
410024       MOVE SPACE TO ZERRMSG                                              
420024                                                                          
430024       IF  RETURN-CODE < 8                                                
440024         IF  TSEL NOT = SPACE                                             
450024                                                                          
460024           CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY SAVE                
470024           IF TSEL = 'I'                                                  
480024             MOVE 'Y' TO TSEL                                             
490024             PERFORM S1-SOP20                                             
500024             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
510024           ELSE IF TSEL = 'C'                                             
520024             IF TOSTATUS = 'STARTED'                                      
530024               PERFORM S1-SOP20                                           
540024             ELSE                                                         
550024               MOVE 'D' TO TSEL                                           
560024               PERFORM S1-SOP20                                           
570024             END-IF                                                       
580024             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
590024           END-IF                                                         
600024           END-IF                                                         
610024           IF ZERRMSG = SPACE                                             
620024             MOVE SPACE TO TSEL                                           
630024             CALL ISPLINK USING TBPUT TBOQ-NAMN                           
640024           END-IF                                                         
650024                                                                          
660024         ELSE IF TSEL = SPACE                                             
670024                                                                          
680024           IF ZCMD NOT = SPACE                                            
690024             CALL ISPLINK USING SETMSG ERROR-ILLEGAL-CMD                  
700024             MOVE ERROR-ILLEGAL-CMD TO ZERRMSG                            
710024           END-IF                                                         
720024                                                                          
730024           MOVE 'I' TO TSEL                                               
740024           MOVE 'Q' TO WS-SEL                                             
750024           PERFORM S1-SOP20                                               
760024                                                                          
770024           CALL ISPLINK USING TBEND TBOQ-NAMN                             
780024           PERFORM S17-SKAPA-ORDER-QUEUE-TABELL                           
790024           CALL W980WSPC USING FQUIT WSRKOD                               
800024         END-IF                                                           
810024         END-IF                                                           
820024                                                                          
830024       ELSE                                                               
840024         CALL ISPLINK USING TBEND TBOQ-NAMN                               
850024       END-IF                                                             
860024                                                                          
870024     END-PERFORM                                                          
880024     .                                                                    
890024     EJECT                                                                
900000 S15-SKAPA-MSL-TABELL SECTION.                                            
910000                                                                          
920000     MOVE ZERO TO ANTAL-RADER                                             
930000     MOVE H-IDPROCESS (HIX) TO P-IDPROCESS TPROCESS                       
940000     MOVE ZERO TO P-LENGD                                                 
950000     INSPECT P-IDPROCESS TALLYING P-LENGD                                 
960000     FOR CHARACTERS BEFORE INITIAL SPACE                                  
970000     ADD 2 TO P-LENGD                                                     
980000                                                                          
990000     MOVE SPACE TO DATA-VAERDE                                            
000001     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
010000         INFO-NAMN-PARM DATA-PARM                                         
020000     IF WSRKOD NOT = SPACE                                                
030000*--    PROCESS FINNS EJ                                                   
040000       CALL ISPLINK USING SETMSG ERROR-PROC-NOT-FOUND                     
050000       MOVE ERROR-PROC-NOT-FOUND TO ZERRMSG                               
060000       SUBTRACT 1 FROM HIX                                                
070000       IF HIX > 0                                                         
080000         CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE               
090000       END-IF                                                             
100000     ELSE                                                                 
110000       MOVE SPACE TO NAMN-VAERDE                                          
120001       CALL W980WSPC USING FGETF WSRKOD P-PARM CONTAINS-PARM              
130000                     NAMN-PARM                                            
140000       IF WSRKOD NOT = SPACE                                              
150000         CALL ISPLINK USING SETMSG ERROR-NO-SUB                           
160000         MOVE ERROR-NO-SUB TO ZERRMSG                                     
170000         SUBTRACT 1 FROM HIX                                              
180000         IF HIX > 0                                                       
190000           CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE             
200000         END-IF                                                           
210000       ELSE                                                               
220000         MOVE HIX TO TB-NR                                                
230000         CALL ISPLINK USING TBCREATE TB-NAMN TB-KEYNAMES                  
240000                        TB-NAMES NOWRITE                                  
250000         MOVE ZERO TO ANTAL-VARV                                          
260000         MOVE ZERO TO SOP20-KDRET                                         
270000         PERFORM UNTIL WSRKOD NOT = SPACE OR SOP20-KDRET  > 4             
280000               OR ANTAL-VARV NOT < 500                                    
290000                                                                          
300000           MOVE NAMN-VAERDE TO TPROCESS                                   
310000           PERFORM S30-HAEMTA-RAD-INFO                                    
320000                                                                          
330000           IF TSNR NOT < MINSNR                                           
340000             CALL ISPLINK USING TBADD TB-NAMN                             
350000             ADD 1 TO ANTAL-RADER                                         
360000           END-IF                                                         
370000                                                                          
380000           MOVE SPACE TO NAMN-VAERDE                                      
390001           CALL W980WSPC USING FGETN WSRKOD P-PARM CONTAINS-PARM          
400000                         NAMN-PARM                                        
410000           ADD 1 TO ANTAL-VARV                                            
420000         END-PERFORM                                                      
430000         IF ANTAL-RADER > 0                                               
440000           CALL ISPLINK USING TBTOP TB-NAMN                               
450000         ELSE                                                             
460000           MOVE P-IDPROCESS TO TPROCESS                                   
470000           CALL ISPLINK USING SETMSG ERROR-NO-SUIT-SUB                    
480000           MOVE ERROR-NO-SUIT-SUB TO ZERRMSG                              
490000           CALL ISPLINK USING TBEND TB-NAMN                               
500000           SUBTRACT 1 FROM HIX                                            
510000           MOVE HIX TO TB-NR                                              
520000           IF HIX > 0                                                     
530000             CALL ISPLINK USING ISP-CONTROL ISP-DISPLAY RESTORE           
540000           END-IF                                                         
550000         END-IF                                                           
560000       END-IF                                                             
570000     END-IF                                                               
580001     CALL W980WSPC USING FSAVE WSRKOD                                     
590000     IF ANTAL-RADER > 0                                                   
600000       MOVE 1 TO ZTDTOP                                                   
610000       PERFORM S40-UPPDATERA-JOB-STATUS                                   
620000     END-IF                                                               
630000     .                                                                    
640000     EJECT                                                                
650000 S16-SKAPA-STARTED-MSL-TABELL SECTION.                                    
660000     SKIP2                                                                
670000     MOVE ZERO TO MINSNR                                                  
680000     MOVE ZERO TO ANTAL-VARV                                              
690000     MOVE ZERO TO ANTAL-RADER                                             
700000     MOVE SPACE TO NAMN-VAERDE                                            
710001     CALL W980WSPC USING FGETF WSRKOD STARTED-LIST-PARM                   
720000                   NULL-PARM NAMN-PARM                                    
730000     IF WSRKOD NOT = SPACE                                                
740000       CALL ISPLINK USING SETMSG ERROR-NO-STARTED                         
750000       MOVE ERROR-NO-STARTED TO ZERRMSG                                   
760000       SUBTRACT 1 FROM HIX                                                
770000     ELSE                                                                 
780000       MOVE HIX TO TB-NR                                                  
790000       CALL ISPLINK USING TBCREATE TB-NAMN TB-KEYNAMES                    
800000                      TB-NAMES NOWRITE                                    
810000       PERFORM UNTIL WSRKOD NOT = SPACE OR ANTAL-VARV NOT < 100           
820000                                                                          
830000         MOVE NAMN-VAERDE TO TPROCESS                                     
840000         PERFORM S30-HAEMTA-RAD-INFO                                      
850000         IF TMTYP NOT > MAXTYP                                            
860000           CALL ISPLINK USING TBADD TB-NAMN                               
870000           ADD 1 TO ANTAL-RADER                                           
880000         END-IF                                                           
890000         ADD 1 TO ANTAL-VARV                                              
900000         MOVE SPACE TO NAMN-VAERDE                                        
910001         CALL W980WSPC USING FGETN WSRKOD STARTED-LIST-PARM               
920000                       NULL-PARM NAMN-PARM                                
930000       END-PERFORM                                                        
940000       IF ANTAL-RADER > 0                                                 
950000         CALL ISPLINK USING TBTOP TB-NAMN                                 
960000       ELSE                                                               
970000         CALL ISPLINK USING SETMSG ERROR-NO-SUIT-TYPE                     
980000         MOVE ERROR-NO-SUIT-TYPE TO ZERRMSG                               
990000       END-IF                                                             
000000     END-IF                                                               
010001     CALL W980WSPC USING FSAVE WSRKOD                                     
020000     IF ANTAL-RADER > 0                                                   
030000       MOVE 1 TO ZTDTOP                                                   
040000       PERFORM S40-UPPDATERA-JOB-STATUS                                   
050000     END-IF                                                               
060000     .                                                                    
070024     EJECT                                                                
080024 S17-SKAPA-ORDER-QUEUE-TABELL SECTION.                                    
090024     SKIP2                                                                
100024     MOVE ZERO TO ANTAL-RADER                                             
110024     MOVE ZERO TO TBOQDISPL-RC                                            
120024     CALL ISPLINK USING TBCREATE TBOQ-NAMN TBOQ-KEYNAMES                  
130024                    TBOQ-NAMES NOWRITE                                    
140024     IF SOP20-KDPROCSTAT = WAITING-STATUS OR STARTED-STATUS               
150024       ADD 1 TO ANTAL-RADER                                               
160024       IF SOP20-KDPROCSTAT = WAITING-STATUS                               
170024         MOVE 'WAITING' TO TOSTATUS                                       
180024       ELSE                                                               
190024         MOVE 'STARTED' TO TOSTATUS                                       
200024       END-IF                                                             
210024       MOVE SOP20-TIAPDAT-SENAST TO TODATE                                
220024       MOVE SOP20-KDANR   TO TANR                                         
230024       CALL ISPLINK USING TBADD TBOQ-NAMN                                 
240024     END-IF                                                               
250024                                                                          
260024     MOVE +0 TO P-LENGD                                                   
270024     INSPECT P-IDPROCESS TALLYING P-LENGD                                 
280024        FOR CHARACTERS BEFORE INITIAL SPACE                               
290024     ADD +2 TO P-LENGD                                                    
300024                                                                          
310024     MOVE 'QUEUED' TO TOSTATUS                                            
320024     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
330024           ACT-QUEUE-PARM BEG-TIAPDAT-ANR-PARM                            
340024     PERFORM UNTIL WSRKOD NOT = SPACE OR ANTAL-RADER NOT < 9000           
350024                                                                          
360024       MOVE BEG-TIAPDAT TO TODATE                                         
370024       MOVE P-ANR   TO TANR                                               
380024       CALL ISPLINK USING TBADD TBOQ-NAMN                                 
390024       ADD 1 TO ANTAL-RADER                                               
400024       CALL W980WSPC USING FGETN WSRKOD P-PARM                            
410024             ACT-QUEUE-PARM BEG-TIAPDAT-ANR-PARM                          
420024     END-PERFORM                                                          
430024     IF ANTAL-RADER > 0                                                   
440024       CALL ISPLINK USING TBTOP TBOQ-NAMN                                 
450024     ELSE                                                                 
460024       CALL ISPLINK USING TBEND TBOQ-NAMN                                 
470024       CALL ISPLINK USING SETMSG ERROR-NO-ORDER-QUEUE                     
480024       MOVE ERROR-NO-ORDER-QUEUE TO ZERRMSG                               
490024     END-IF                                                               
500024     CALL W980WSPC USING FQUIT WSRKOD                                     
510024     .                                                                    
520000     EJECT                                                                
530000 S20-UPPDATERA-MSL-TABELL SECTION.                                        
540000     SKIP2                                                                
550000     CALL ISPLINK USING TBTOP TB-NAMN                                     
560000     CALL ISPLINK USING TBSKIP TB-NAMN ZTDTOP                             
570000     MOVE 1 TO ANTAL-VARV                                                 
580031     PERFORM UNTIL RETURN-CODE NOT = 0 OR ANTAL-VARV NOT < 30             
590000       IF TSNR NOT < MINSNR                                               
600000         PERFORM S25-UPPDAT-TAB-RAD                                       
610000         ADD 1 TO ANTAL-VARV                                              
620000       END-IF                                                             
630000       CALL ISPLINK USING TBSKIP TB-NAMN                                  
640000     END-PERFORM                                                          
650000     CALL ISPLINK USING TBTOP TB-NAMN                                     
660000     CALL ISPLINK USING TBSKIP TB-NAMN ZTDTOP                             
670000     .                                                                    
680000     EJECT                                                                
690000 S25-UPPDAT-TAB-RAD  SECTION.                                             
700000     SKIP2                                                                
710000     MOVE TPROCESS TO NAMN-VAERDE                                         
720000     MOVE ZERO TO NAMN-LENGD                                              
730000     INSPECT NAMN-VAERDE TALLYING NAMN-LENGD                              
740000     FOR CHARACTERS BEFORE INITIAL SPACE                                  
750000     ADD 2 TO NAMN-LENGD                                                  
760000     PERFORM S30-HAEMTA-RAD-INFO                                          
770000     CALL ISPLINK USING TBPUT TB-NAMN                                     
780000     .                                                                    
790000     EJECT                                                                
800000 S30-HAEMTA-RAD-INFO SECTION.                                             
810000     SKIP2                                                                
820000     MOVE 'J' TO SOP20-KDSOPFUNK                                          
830000     MOVE NAMN-VAERDE TO SOP20-IDPROCESS                                  
840000     MOVE ZERO TO SOP20-TIAPDAT                                           
850000     PERFORM S2-CALL-SOP20                                                
860000                                                                          
870000     IF SOP20-SYSTEM-PROCESS                                              
880000       MOVE 'SYS' TO TMTYP                                                
890000     ELSE IF SOP20-ROUTINE-PROCESS                                        
900000       MOVE 'RTN' TO TMTYP                                                
910000     ELSE IF SOP20-JOB-PROCESS                                            
920000       MOVE 'JOB' TO TMTYP                                                
930000     ELSE IF SOP20-PROCEDURE-PROCESS                                      
940000       MOVE 'PRO' TO TMTYP                                                
950000     ELSE                                                                 
960000       MOVE SPACE TO TMTYP                                                
970000     END-IF                                                               
980000     END-IF                                                               
990000     END-IF                                                               
000000     END-IF                                                               
010000                                                                          
020000     MOVE SPACE TO TNOTE                                                  
030000     IF SOP20-KDPROCSTAT = PASSIVE-STATUS                                 
040000        MOVE 'PASSIVE' TO TSTATUS                                         
050000        MOVE '0'       TO TSNR                                            
060000        MOVE SPACE TO TNOTE                                               
070000     ELSE IF SOP20-KDPROCSTAT = ENDED-STATUS                              
080000        MOVE 'ENDED AT' TO TSTATUS                                        
090000        MOVE '1'       TO TSNR                                            
100000        MOVE SOP20-TIMINUT-STOPP TO TID-ZONAT                             
110000        STRING  TID-HH ':' TID-MM DELIMITED BY SIZE                       
120000            INTO TNOTE                                                    
130000     ELSE IF SOP20-KDPROCSTAT = WAITING-STATUS                            
140000        MOVE '2'       TO TSNR                                            
150000        IF SOP20-TEPRED = SPACE                                           
160000          MOVE 'WAIT ' TO TSTATUS                                         
170000        ELSE                                                              
180000          MOVE 'WAIT FOR' TO TSTATUS                                      
190000          STRING  SOP20-TEPRED     DELIMITED BY SIZE                      
200000              INTO TNOTE                                                  
210000        END-IF                                                            
220000     ELSE IF SOP20-KDPROCSTAT = STARTED-STATUS                            
230000        MOVE 'START AT' TO TSTATUS                                        
240000        MOVE '3'       TO TSNR                                            
250000        MOVE SOP20-TIMINUT-START TO TID-ZONAT                             
260000        STRING  TID-HH ':' TID-MM DELIMITED BY SIZE                       
270000            INTO TNOTE                                                    
280000     END-IF                                                               
290000     END-IF                                                               
300000     END-IF                                                               
310000     END-IF                                                               
320000                                                                          
330000     MOVE SOP20-KDPROCSTRT TO TSTYP                                       
340000                                                                          
350005     IF SOP20-TERES NOT = SPACE                                           
360013       MOVE ZERO TO TALLY-RAEKN                                           
370018       INSPECT SOP20-TEATTN TALLYING TALLY-RAEKN                          
380005               FOR CHARACTERS BEFORE INITIAL 'F.'                         
390018       IF TALLY-RAEKN = LENGTH OF SOP20-TEATTN                            
400005*        -- RESURS ANVÄNDS OCH FREE EJ GJORT                              
410005         MOVE 'RES' TO TRES                                               
420015       ELSE                                                               
430015         MOVE SPACE TO TRES                                               
440005       END-IF                                                             
450016     ELSE                                                                 
460016       MOVE SPACE TO TRES                                                 
470005     END-IF                                                               
480005                                                                          
490000     MOVE SPACE TO TATTN                                                  
500000     MOVE 1 TO PTR                                                        
510000                                                                          
520000     IF SOP20-TITMPDUR NOT = 0                                            
530000       MOVE 'TMP-JCL' TO TATTN                                            
540000       MOVE 8 TO PTR                                                      
550000     END-IF                                                               
560000                                                                          
570000     IF SOP20-FLHOLD = JA                                                 
580000       IF PTR > 1                                                         
590000         STRING '. ' DELIMITED BY SIZE                                    
600000                'HELD' DELIMITED BY SIZE                                  
610000              INTO TATTN WITH POINTER PTR                                 
620000       ELSE                                                               
630000         MOVE 'HELD' TO TATTN                                             
640000         MOVE 5 TO PTR                                                    
650000       END-IF                                                             
660000     END-IF                                                               
670000     IF PTR > 1                                                           
680000       STRING '. ' DELIMITED BY SIZE                                      
690000              SOP20-TEATTN DELIMITED BY SIZE                              
700000            INTO TATTN WITH POINTER PTR                                   
710000     ELSE                                                                 
720000       STRING SOP20-TEATTN DELIMITED BY SIZE                              
730000            INTO TATTN WITH POINTER PTR                                   
740000     END-IF                                                               
750000                                                                          
760000     IF SOP20-TEATTN NOT = SPACE AND AUTO-SWITCH = JA                     
770000       MOVE SOP20-TEATTN TO TMP-ATTN                                      
780000       IF TMP-ATTN-1-5 = 'ABEND'                                          
790000         MOVE NEJ TO AUTO-SWITCH                                          
800000         MOVE MSG-AUTO-INTERRUPTED TO ZERRMSG                             
810000       END-IF                                                             
820000     END-IF                                                               
830000                                                                          
840000     .                                                                    
850000     EJECT                                                                
860000 S40-UPPDATERA-JOB-STATUS SECTION.                                        
870000     SKIP2                                                                
880000     CALL ISPLINK USING TBTOP TB-NAMN                                     
890000     CALL ISPLINK USING TBSKIP TB-NAMN ZTDTOP                             
900000     MOVE 1 TO ANTAL-VARV                                                 
910031     PERFORM UNTIL RETURN-CODE NOT = 0 OR ANTAL-VARV NOT < 30             
920000       IF TSNR NOT < MINSNR                                               
930000         PERFORM S45-ETT-JOB-STATUS                                       
940000         ADD 1 TO ANTAL-VARV                                              
950000       END-IF                                                             
960000       CALL ISPLINK USING TBSKIP TB-NAMN                                  
970000     END-PERFORM                                                          
980000     CALL ISPLINK USING TBTOP TB-NAMN                                     
990000     CALL ISPLINK USING TBSKIP TB-NAMN ZTDTOP                             
000000     .                                                                    
010000     EJECT                                                                
020000 S45-ETT-JOB-STATUS  SECTION.                                             
030000     SKIP2                                                                
040000     IF TSNR = '3' AND TMTYP = 'JOB'                                      
050000       MOVE TPROCESS TO STATUS-JOBNAMN                                    
060025       IF DB-STATUS-VAERDE = 'TEST'                                       
070000         MOVE 'T' TO STATUS-JOBNAMN-POS5                                  
080000       END-IF                                                             
090000       CALL ISPLINK USING ISP-SELECT L-STATUS-CMD STATUS-CMD              
100025       CALL ISPLINK USING VGET N-SOPJOBST ISP-SHARED                      
110000                                                                          
120000       MOVE SPACE TO TMP-ATTN                                             
130000                                                                          
140000       IF SOPJOBST = 'M' THEN                                             
150000         IF TSTYP = 'REL'                                                 
160000           MOVE 'MULT JOBS' TO TMP-ATTN                                   
170000         END-IF                                                           
180000       ELSE IF SOPJOBST = 'O'                                             
190013         MOVE ZERO TO TALLY-RAEKN                                         
200013         INSPECT TATTN TALLYING TALLY-RAEKN                               
210005                 FOR CHARACTERS BEFORE INITIAL 'ABEND'                    
220017         IF TALLY-RAEKN = L-TATTN                                         
230000           MOVE 'ON OUTQ JCLERR?' TO TMP-ATTN                             
240000         END-IF                                                           
250000       ELSE IF SOPJOBST = 'I'                                             
260000         MOVE 'ON INQ' TO TMP-ATTN                                        
270000       ELSE IF SOPJOBST = '?'                                             
280013         MOVE ZERO TO TALLY-RAEKN                                         
290013         INSPECT TATTN TALLYING TALLY-RAEKN                               
300005                 FOR CHARACTERS BEFORE INITIAL 'ABEND'                    
310017         IF TALLY-RAEKN = L-TATTN                                         
320005           MOVE 'NO JOB JCLERR?' TO TMP-ATTN                              
330000         END-IF                                                           
340000       ELSE IF SOPJOBST = 'H'                                             
350000         MOVE 'JES2 HOLD' TO TMP-ATTN                                     
360000       END-IF                                                             
370000       END-IF                                                             
380000       END-IF                                                             
390000       END-IF                                                             
400000       END-IF                                                             
410006                                                                          
420000       IF TMP-ATTN NOT = SPACE                                            
430006         IF TRES NOT = SPACE                                              
440014           STRING TRES  '. ' TMP-ATTN DELIMITED BY SIZE                   
450013           INTO TEMP-STRING                                               
460014           MOVE TEMP-STRING TO TMP-ATTN                                   
470006         END-IF                                                           
480000         IF TATTN = SPACE                                                 
490000           MOVE TMP-ATTN TO TATTN                                         
500000         ELSE                                                             
510020           MOVE ZERO TO TALLY-RAEKN                                       
520019           INSPECT TATTN TALLYING TALLY-RAEKN                             
530019                   FOR CHARACTERS BEFORE INITIAL '.  '                    
540019           IF TALLY-RAEKN < L-TATTN                                       
550022             MOVE SPACE TO TATTN (TALLY-RAEKN + 1 : 1)                    
560019           END-IF                                                         
570000           STRING TATTN  DELIMITED BY '  '                                
580000              '. ' DELIMITED BY SIZE                                      
590000              TMP-ATTN  DELIMITED BY SIZE                                 
600000              INTO TATTN                                                  
610000         END-IF                                                           
620000       END-IF                                                             
630000       CALL ISPLINK USING TBPUT TB-NAMN                                   
640000                                                                          
650000       IF AUTO-SWITCH = JA AND SOPJOBST NOT = 'I'                         
660000           AND TATTN NOT = SPACE                                          
670000         MOVE NEJ TO AUTO-SWITCH                                          
680000         MOVE MSG-AUTO-INTERRUPTED TO ZERRMSG                             
690000       END-IF                                                             
700000     END-IF                                                               
710000     .                                                                    
720000     EJECT                                                                
730000 S46-INFO-JOB-STATUS SECTION.                                             
740000     SKIP2                                                                
750000     IF WSTATUS = 'S' AND WMTYP = 'J'                                     
760000       MOVE WPROCESS TO STATUS-JOBNAMN                                    
770025       IF DB-STATUS-VAERDE = 'TEST'                                       
780000         MOVE 'T' TO STATUS-JOBNAMN-POS5                                  
790000       END-IF                                                             
800000       CALL ISPLINK USING ISP-SELECT L-STATUS-CMD STATUS-CMD              
810025       CALL ISPLINK USING VGET N-SOPJOBST ISP-SHARED                      
820000                                                                          
830000       MOVE SPACE TO TMP-ATTN                                             
840000                                                                          
850000       IF SOPJOBST = 'M' THEN                                             
860000         IF TSTYP = 'REL'                                                 
870000           MOVE 'MULT JOBS' TO TMP-ATTN                                   
880000         END-IF                                                           
890000       ELSE IF SOPJOBST = 'O'                                             
900013         MOVE ZERO TO TALLY-RAEKN                                         
910013         INSPECT WATTN TALLYING TALLY-RAEKN                               
920007                 FOR CHARACTERS BEFORE INITIAL 'ABEND'                    
930013         IF TALLY-RAEKN = 30                                              
940000           MOVE 'ON OUTQ JCLERR?' TO TMP-ATTN                             
950000         END-IF                                                           
960000       ELSE IF SOPJOBST = 'I'                                             
970000         MOVE 'ON INQ' TO TMP-ATTN                                        
980000       ELSE IF SOPJOBST = '?'                                             
990013         MOVE ZERO TO TALLY-RAEKN                                         
000013         INSPECT WATTN TALLYING TALLY-RAEKN                               
010007                 FOR CHARACTERS BEFORE INITIAL 'ABEND'                    
020013         IF TALLY-RAEKN = 30                                              
030000           MOVE 'NO JOB JCLERR?' TO TMP-ATTN                              
040000         END-IF                                                           
050000       ELSE IF SOPJOBST = 'H'                                             
060000         MOVE 'JES2 HOLD' TO TMP-ATTN                                     
070000       END-IF                                                             
080000       END-IF                                                             
090000       END-IF                                                             
100000       END-IF                                                             
110000       END-IF                                                             
120000       IF TMP-ATTN NOT = SPACE                                            
130010         IF TRES NOT = SPACE                                              
140014           STRING TRES  '. ' TMP-ATTN DELIMITED BY SIZE                   
150013           INTO TEMP-STRING                                               
160014           MOVE TEMP-STRING TO TMP-ATTN                                   
170010         END-IF                                                           
180000         IF WATTN = SPACE                                                 
190000           MOVE TMP-ATTN TO WATTN                                         
200000         ELSE                                                             
210000           STRING WATTN  DELIMITED BY '  '                                
220000                  '. '  DELIMITED BY SIZE                                 
230000                  TMP-ATTN  DELIMITED BY SIZE                             
240000              INTO WATTN                                                  
250000         END-IF                                                           
260000       END-IF                                                             
270000                                                                          
280000     END-IF                                                               
290000     .                                                                    
300000     EJECT                                                                
310000 Z-FINIT     SECTION.                                                     
320000     SKIP2                                                                
330000     IF SOP-OPPNAD = JA                                                   
340000       MOVE '9' TO SOP20-KDSOPFUNK                                        
350000       PERFORM S2-CALL-SOP20                                              
360000     END-IF                                                               
370000     IF JCL-ALLOCATED = JA                                                
380000       CALL ISPLINK USING ISP-SELECT L-FREE-CMD FREE-CMD                  
390000     END-IF                                                               
400000     CALL ISPLINK USING VRESET                                            
410000     .                                                                    
420000     EJECT                                                                
421040     EJECT                                                                
430040*    -COPY WY2000P1                                                       
