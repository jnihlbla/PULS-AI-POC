000100*** EDIT ALLOWED                                                          
000200 01  T335R301.                                                            
000300*                    NP REQUISITIONS FROM PULS                            
000400*                        ANV I PGM   W11450                               
000400*                                                                         
000600     03  IDRT                   PIC   X(3)  VALUE '301'.                  
000500*                                    RECORD TYPE                          
000500*                                                                         
000600     03  COMMON-AREA.                                                     
000700         05 CDTYPE-REQ          PIC   X(2)  VALUE 'PR'.                   
000500*                                    REQ TYPE                             
000800         05 IDPITEM-GRP.                                                  
003000            07 IDPITEM          PIC   X(20).                              
003000*                                    PURCHASED ITEM NO  JUST R            
003000            07 CD-IDPITEM       PIC   X(1)  VALUE 'V'.                    
003800*                                    PURCHASED ITEM QUALIF                
000700         05 NMPITEM             PIC   X(25) VALUE SPACE.                  
000700*                                    PARTNAME GB                          
000700         05 IDPORG              PIC   X(4)  VALUE 'VCAS'.                 
003800*                                    PURCHASE ORGANIZATION                
000700         05 IDHANDLR            PIC   9(4).                               
000700*                                    IDINK                                
000700         05 FILLER              PIC   X(1)  VALUE SPACE.                  
000700         05 TXNOTES-REQ1        PIC   X(70) VALUE SPACE.                  
000700*                                    NOTES LINE 1                         
000700         05 TXNOTES-REQ2        PIC   X(70) VALUE SPACE.                  
000700*                                    NOTES LINE 2                         
000700         05 FILLER              PIC   X(4)  VALUE SPACE.                  
000700         05 FILLER              PIC   9(4)  VALUE ZERO.                   
000700         05 NMHANDLR-ISSUER     PIC   X(25) VALUE SPACE.                  
000700*                                    HANDLER NAME                         
000700         05 IDPHONE-ISSUER      PIC   X(20) VALUE SPACE.                  
000700*                                    TELEPHONE NO                         
000700         05 IDSECTN-ISSUER      PIC   X(6)  VALUE SPACE.                  
000700*                                    SECTION                              
000700         05 IDUSERID-ISSUER     PIC   X(8)  VALUE SPACE.                  
000700*                              CDS ID FOR ISSUER   NB CDS ID              
000700         05 FLPLANT-BUYER       PIC   X(1)  VALUE SPACE.                  
000700         05 RVI-AREA.                                                     
003000            07 CDREQ-CONTROL    PIC   X(2)  VALUE SPACE.                  
003000            07 IDCONSIG         PIC   9(5)  VALUE ZERO.                   
003000            07 CD-IDCONSIG      PIC   X(1)  VALUE SPACE.                  
003000            07 CDREQ-ACCOUNT.                                             
003000               09 CDACCOUNT-LOC PIC   X(2)  VALUE SPACE.                  
003000               09 CDACCOUNT-ATT PIC   X(1)  VALUE SPACE.                  
003000               09 IDORDNO-PTA   PIC   X(6)  VALUE SPACE.                  
003000               09 FILLER        REDEFINES IDORDNO-PTA.                    
003000                  11 FILLER     PIC   X(4).                               
003000                  11 IDUSER-RVI PIC   X(2).                               
003000               09 IDCONSIG-RVI  PIC   X(2)  VALUE SPACE.                  
003000            07 FLCOBL-ALLOWED   PIC   X(1)  VALUE 'Y'.                    
003000            07 FILLER           PIC   X(30) VALUE SPACE.                  
003800*                                                                         
000600     03  NP-AREA.                                                         
000700         05 NP-IDUSER           PIC   X(5)  VALUE SPACE.                  
000700*                   MATERIAL USER IDENTITY  GSDB ID = 'BP2TW'             
000700         05 NP-IDREQ            PIC   X(7)  VALUE SPACE.                  
000700         05 FILLER              PIC   9(5)  VALUE ZERO.                   
000700         05 NP-CDUOM            PIC   X(3)  VALUE SPACE.                  
000700*                                    UNIT OF MEASUREMENT    PCE           
000700         05 FILLER              PIC   X(8)  VALUE SPACE.                  
000700         05 NP-IDISSUE          PIC   X(3)  VALUE SPACE.                  
000700         05 NP-IDDRAWNG         PIC   X(20) VALUE SPACE.                  
000700         05 FILLER              PIC   X(31) VALUE SPACE.                  
000700         05 FILLER              PIC   9(7)  VALUE ZERO.                   
000700         05 NP-TIPROD-DATE      PIC   9(6)  VALUE ZERO.                   
000700*                                    PROD DATE YYMMDD                     
000700         05 NP-QTPITEM-YEAR     PIC   9(9)  VALUE ZERO.                   
000700*                                    PART QUANTITY PER YEAR               
000700         05 FILLER              PIC   9(17) VALUE ZERO.                   
000700         05 FILLER              PIC   9(10) VALUE ZERO.                   
000700         05 NP-QTPITEM-ORDER    PIC   9(9)  VALUE ZERO.                   
000700*                                    ONE SHOT QUANTITY                    
000700         05 FILLER              PIC   9(17) VALUE ZERO.                   
000700         05 FILLER              PIC   9(05) VALUE ZERO.                   
000700         05 NP-TIPRE-DEL-DATE-1 PIC   9(6)  VALUE ZERO.                   
000700         05 NP-QTPRE-DEL-1      PIC   9(4)  VALUE ZERO.                   
000700         05 NP-TIPRE-DEL-2      PIC   9(4)  VALUE ZERO.                   
000700         05 NP-TIPRE-DEL-DATE-2 PIC   9(6)  VALUE ZERO.                   
000700         05 NP-QTPRE-DEL-2      PIC   9(4)  VALUE ZERO.                   
000700         05 NP-TIPRE-DEL-3      PIC   9(4)  VALUE ZERO.                   
000700         05 NP-TIPRE-DEL-DATE-3 PIC   9(6)  VALUE ZERO.                   
000700         05 NP-QTPRE-DEL-3      PIC   9(4)  VALUE ZERO.                   
000700         05 NP-TISAMPLE         PIC   9(4)  VALUE ZERO.                   
000700         05 NP-TISAMPLE-DATE    PIC   9(6)  VALUE ZERO.                   
000700         05 NP-QTSAMPLE         PIC   9(4)  VALUE ZERO.                   
000700         05 FILLER              PIC   X(42) VALUE SPACE.                  
000700         05 NP-FLCOBL-REQUIRED  PIC   X(1)  VALUE SPACE.                  
003800*                                           VALUE = 'Y'                   
000700         05 FILLER              PIC   X(30) VALUE SPACE.                  
003800*                                                                         
000600     03  FORTS                  PIC   X(565) VALUE SPACE.                 
003800*                                                                         
000600     03  CC-AREA.                                                         
000700         05 CC-FLMTRL           PIC   X(1)  VALUE SPACE.                  
000700*                             SCREEN FLAG    ALWAYS = 'N'                 
000700         05 CC-IDUSER           PIC   X(5)  VALUE SPACE.                  
000700*                             MATERIAL USER IDENTITY                      
000700         05 CC-IDREQ            PIC   X(7)  VALUE SPACE.                  
000700         05 CC-IDSUPPL          PIC   9(5)  VALUE ZERO.                   
000700         05 CC-CD-IDSUPPL       PIC   X(1)  VALUE SPACE.                  
000700         05 CC-CDMODE-ORDER     PIC   X(2)  VALUE SPACE.                  
000700*                                    ORDER MODE 'SA' OR 'BL'              
000700         05 CC-IDORDER-GRP      PIC   X(7)  VALUE SPACE.                  
000700         05 CC-IDSUFFIX-ORDER   PIC   9(3)  VALUE ZERO.                   
000700         05 CC-TIEXIT-WEEK      PIC   X(4)  VALUE SPACE.                  
000700*                                    YYWW CANCEL WEEK                     
000700         05 CC-FLTOTAL-CANCEL   PIC   X(1)  VALUE SPACE.                  
000700*                                    ITEM/USER TOTAL CANCEL ='Y'          
000700         05 CC-CDUOM            PIC   X(3)  VALUE SPACE.                  
000700*                                    UNIT OF MEASUREMENT  SPACE           
000700         05 CC-QTPITEM-CANCEL   PIC   9(9)  VALUE ZERO.                   
000700         05 CC-QTPITEM-LAST-DEL PIC   9(9)  VALUE ZERO.                   
000700         05 CC-QTPITEM-REMAIN   PIC   9(9)  VALUE ZERO.                   
000700         05 CC-QTPITEM-STOCK    PIC   9(9)  VALUE ZERO.                   
000700         05 FILLER              PIC  X(145) VALUE SPACE.                  
