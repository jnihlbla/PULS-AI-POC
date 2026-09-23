000010*** EDIT ALLOWED                                                          
000001 01  W23355.                                                              
000002*                                                                         
000003*                               DAGLIGA TRANSAR TILL CARPAC               
000004*                                    ****                                 
000005*                               W233 VNWP                                 
000006*                                    ****                                 
000007     03  FZ2RECT                PIC  X(04).                               
000008*                                             RECORD TYPE                 
000009*                                                                         
000010     03  FZ2PART.                                                         
000011         05  IDARTNR            PIC  9(07).                               
000012         05  FILLER             PIC  X(05).                               
000013*                                                                         
000014     03  FZ2PART-8              REDEFINES FZ2PART.                        
000015         05  IDARTNR-8          PIC  9(08).                               
000016         05  FILLER             PIC  X(04).                               
000017*                                             PART NUMBER                 
000018*                                                                         
000019     03  FZ2COMP.                                                         
000020         05  IDPROENH-1         PIC  X(08).                               
000021         05  FILLER             PIC  X(04).                               
000022*                                             COMPOSITION                 
000023*                                             1ST EQUIPMENT               
000024*                                             (IDPROENH)                  
000025*                                                                         
000026     03  FZ2PGRP                PIC  9(02).                               
000027*                                             PRODUCTGROUP                
000028*                                             (KDPRODSL)                  
000029*                                                                         
000030     03  FZ2FGRP                PIC  9(04).                               
000031*                                             FUNCTIONGROUP               
000032*                                             (IDFKNGRP)                  
000033*                                                                         
000034     03  FZ2DESC.                                                         
000035        05  BEART-ENG           PIC  X(25).                               
000036        05  FILLER              PIC  X(05).                               
000037*                                             DESCRIPTION ENGLISH         
000038*                                                                         
000039     03  FZ2PROJ                PIC  X(04).                               
000040*                                             PROJECT                     
000041*                                             (IDPROJ)                    
000042*                                                                         
000047     03  FZ2PEND                PIC  9(06).                               
000048*                                             YYMMDD PRODUCTION           
000049*                                             END DATE                    
000050*                                             (TIURPROD)                  
000051*                                                                         
000052     03  FZ2PSTA                PIC  9(06).                               
000053*                                             YYMMDD PRODUCTION           
000054*                                             START DATE                  
000055*                                             (TIFINLV)                   
000056     03  FZ2PLAN                PIC  9(04).                               
000057*                                             PARTS PLANNER               
000058*                                             (IDBERED)                   
000059*                                                                         
000060     03  FZ2ADVY                PIC  9(09)-.                              
000061*                                             ADV. YEAR QTY               
000062*                                             (KVPROG)                    
000063*                                                                         
000064     03  FZ2UOFM                PIC  X(02).                               
000065*                                             UNIT OF MEASURE             
000066*                                             (KDSORT)                    
000067*                                                                         
000073     03  FZ2AO1                 PIC  9(06).                               
000074*                                             ALT ORDER 1                 
000075*                                             (IDAO-1) 6V TECK            
000076*                                                                         
000077     03  FZ2AON1                PIC  9(04).                               
000078*                                             ALT ORDER 1 VOLGNR          
000079*                                             (IDAO-1) 4H TECK            
000080*                                                                         
000089     03  FZ2SUP1                PIC  X(40).                               
000090*                                             SUPPL. DESCR. 1             
000091*                                             (KDNOTTYP-3)                
000092*                                             (BERNOT)                    
000093*                                                                         
000094     03  FZ2SUP2                PIC  X(20).                               
000095*                                             SUPPL. DESCR. 2             
000096*                                             (KDNOTTYP-6)                
000097*                                             (VERNOT)                    
000098*                                                                         
000099     03  FZ2DRAW                PIC  9(10).                               
000100*                                             DRAWINGNR                   
000101*                                             (IDRITN)                    
000102*                                                                         
000103     03  FZ2CA01                PIC  X(05).                               
000104*                                             CATALOGUS-1                 
000105*                                             (IDKAT-1)                   
000106*                                                                         
000107     03  FZ2CA02                PIC  X(05).                               
000108*                                             CATALOGUS-2                 
000109*                                             (IDKAT-2)                   
000110*                                                                         
000111     03  FZ2CA03                PIC  X(05).                               
000112*                                             CATALOGUS-3                 
000113*                                             (IDKAT-3)                   
000114*                                                                         
000115     03  FZ2PACK                PIC  9(02).                               
000116*                                             PACKING CODE                
000117*                                             (BEFT)                      
000118*                                                                         
000119     03  FZ2PI01                PIC  X(30).                               
000120*                                             PACKING INSTRUCT.           
000121*                                             (    ) BL                   
000122*                                                                         
000123     03  FZ2PSIZ                PIC  9(06).                               
000124*                                             CUST. ORDER                 
000125*                                             PACKSIZE                    
000126*                                             (KVQPACK-1)                 
000127*                                                                         
000128     03  FZ2GRC                 PIC  X(03).                               
000129*                                             GRC                         
000130*                                             (KDGK)                      
000131*                                                                         
000132     03  FZ2SUPP                PIC  X(06).                               
000133*                                             SUPPLIER                    
000134*                                             (IDLEVNR)                   
000135*                                                                         
000136     03  FZ2BELEVART            PIC  X(25).                               
000137*                                             SUPPL. PART DESC.           
000138*                                             (BELEVART)                  
000139*                                                                         
000200     03  FILLER                 PIC  X(17).                               
000300*                                                                         
000400***END COPY W23355   LENTH=280                                            
