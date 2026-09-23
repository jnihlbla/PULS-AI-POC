000100 01  EMB-W461S014.                                                        
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 EMBALLAGE INFO TILL NOAC                
000400     03 EMB-SOR0-IDDISTR     PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 EMB-SOR0-IDKUNDNR    PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 EMB-SOR0-IDRONR      PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 EMB-SOR0-TIRODAT     PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (≈≈MMDD)         
001200     03 EMB-SOR0-IDPTYP      PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 EMB-SOR0-IDLOPNR     PIC S9(5)           COMP-3.                  
001500*                                 L÷PNUMMER          IDLOPNR-002          
001600     03 EMB-W461014.                                                      
001700*                                 EMBALLAGEPROFORMA                       
001800*                                 TILL NOAC PT-014                        
001900        05 EMB-IDPTYP        PIC X(3).                                    
002000*                                 POSTTYP                                 
002100        05 EMB-IDDISTR       PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300        05 EMB-IDKUNDNR      PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500        05 EMB-IDORDNR       PIC S9(7)           COMP-3.                  
002600*                                 ORDERNR             IDORDNR-002         
002700        05 EMB-IDDC          PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900        05 EMB-KDFAKTYP      PIC X.                                       
003000*                                 FAKTURATYP                              
003100        05 EMB-IDFAKT        PIC S9(7)           COMP-3.                  
003200*                                 FAKTURANUMMER                           
003300        05 EMB-TIFAKT        PIC S9(7)           COMP-3.                  
003400*                                 FAKTURERINGSDATUM (≈≈MMDD)              
003500        05 EMB-KDPALL        PIC X.                                       
003600*                                 PALLTYP                                 
003700        05 EMB-KVPALL        PIC S9(5)           COMP-3.                  
003800*                                 ANTAL PALLAR         KVPALL-002         
003900        05 EMB-KVKRAG        PIC S9(5)           COMP-3.                  
004000*                                 ANTAL KRAGAR                            
004100        05 EMB-KVLOCK        PIC S9(5)           COMP-3.                  
004200*                                 ANTAL  LOCK                             
004300        05 FILLER            PIC X(6).                                    
004400*** END COPY W461S014    LENGTH=62                                        
