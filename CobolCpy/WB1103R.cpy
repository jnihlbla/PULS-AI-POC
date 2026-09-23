000100*** EDIT ALLOWED                                                          
000200 01  XLS-RUB-WB1103.                                                      
000300*                                 RUBRIKRAD I XLS-FIL                     
000310     03 XLS-RUB-IDARTNR      PIC X(7)   VALUE 'Part nr'.                  
000320     03 XLS-RUB-TAB01        PIC X      VALUE X'05'.                      
000400     03 XLS-RUB-KDANNULL     PIC X(11)  VALUE 'Part status'.              
000500     03 XLS-RUB-TAB02        PIC X      VALUE X'05'.                      
000800     03 XLS-RUB-KDARTTYP     PIC X(9)   VALUE 'Part type'.                
000900     03 XLS-RUB-TAB03        PIC X      VALUE X'05'.                      
001000     03 XLS-RUB-TEARTUTFG    PIC X(13)  VALUE 'Part validity'.            
001100     03 XLS-RUB-TAB04        PIC X      VALUE X'05'.                      
001200     03 XLS-RUB-BEART        PIC X(9)   VALUE 'Part name'.                
001300     03 XLS-RUB-TAB05        PIC X      VALUE X'05'.                      
001400     03 XLS-RUB-TENOTE       PIC X(16)  VALUE 'Part description'.         
001500     03 XLS-RUB-TAB06        PIC X      VALUE X'05'.                      
001510     03 XLS-RUB-IDARTNR-OFARG PIC X(17)                                   
001511                              VALUE 'Part nr uncolored'.                  
001520     03 XLS-RUB-TAB07        PIC X      VALUE X'05'.                      
001530     03 XLS-RUB-KDFARGST     PIC X(12)  VALUE 'Color status'.             
001540     03 XLS-RUB-TAB08        PIC X      VALUE X'05'.                      
001550     03 XLS-RUB-IDPSLAG      PIC X(12)  VALUE 'Product type'.             
001560     03 XLS-RUB-TAB09        PIC X      VALUE X'05'.                      
001570     03 XLS-RUB-BETYP        PIC X(8)   VALUE 'Car type'.                 
001580     03 XLS-RUB-TAB10        PIC X      VALUE X'05'.                      
001590     03 XLS-RUB-IDFKNGRP     PIC X(5)   VALUE 'Group'.                    
001591     03 XLS-RUB-TAB11        PIC X      VALUE X'05'.                      
001592     03 XLS-RUB-IDKDPPOS     PIC X(3)   VALUE 'POS'.                      
001593     03 XLS-RUB-TAB12        PIC X      VALUE X'05'.                      
001594     03 XLS-RUB-IDAOT        PIC X(5)   VALUE 'CO nr'.                    
001595     03 XLS-RUB-TAB13        PIC X      VALUE X'05'.                      
001596     03 XLS-RUB-TIAOINF-AAVV PIC X(8)   VALUE 'CO intro'.                 
001597     03 XLS-RUB-TAB14        PIC X      VALUE X'05'.                      
001600     03 XLS-RUB-IDAOTUTG     PIC X(8)   VALUE 'CO issue'.                 
001700     03 XLS-RUB-TAB15        PIC X      VALUE X'05'.                      
001701     03 XLS-RUB-IDUPPDKU     PIC X(5)   VALUE 'KU nr'.                    
001702     03 XLS-RUB-TAB16        PIC X      VALUE X'05'.                      
001703     03 XLS-RUB-TESTATUPP    PIC X(9)   VALUE 'KU status'.                
001704     03 XLS-RUB-TAB17        PIC X      VALUE X'05'.                      
001705     03 XLS-RUB-BEANST-KU    PIC X(7)   VALUE 'KU resp'.                  
001706     03 XLS-RUB-TAB18        PIC X      VALUE X'05'.                      
001707     03 XLS-RUB-BEASSTYP     PIC X(9)   VALUE 'Ass. type'.                
001708     03 XLS-RUB-TAB19        PIC X      VALUE X'05'.                      
001709     03 XLS-RUB-IDUPPDSU     PIC X(5)   VALUE 'SU nr'.                    
001710     03 XLS-RUB-TAB20        PIC X      VALUE X'05'.                      
001711     03 XLS-RUB-BEUPPDSU     PIC X(8)   VALUE 'SU Title'.                 
001712     03 XLS-RUB-TAB21        PIC X      VALUE X'05'.                      
001713     03 XLS-RUB-BEANST-SU    PIC X(7)   VALUE 'SU resp'.                  
001714     03 XLS-RUB-TAB22        PIC X      VALUE X'05'.                      
001715     03 XLS-RUB-IDPROJK      PIC X(7)   VALUE 'Project'.                  
001716     03 XLS-RUB-TAB23        PIC X      VALUE X'05'.                      
001717     03 XLS-RUB-IDPSS        PIC X(3)   VALUE 'PSS'.                      
001718     03 XLS-RUB-TAB24        PIC X      VALUE X'05'.                      
001719     03 XLS-RUB-FLTPDWKPH1   PIC X(10)  VALUE 'PPAP order'.               
001720     03 XLS-RUB-TAB25        PIC X      VALUE X'05'.                      
001721     03 XLS-RUB-FLRPULS      PIC X(9)   VALUE 'Reg. PULS'.                
001722     03 XLS-RUB-TAB26        PIC X      VALUE X'05'.                      
001723     03 XLS-RUB-KDFRPTYP     PIC X(14)  VALUE 'Pack type(S/P)'.           
001724     03 XLS-RUB-TAB27        PIC X      VALUE X'05'.                      
001725     03 XLS-RUB-KDEMBKOD-2   PIC X(11)  VALUE 'Pack status'.              
001726     03 XLS-RUB-TAB28        PIC X      VALUE X'05'.                      
001727     03 XLS-RUB-BETEXT-OTP   PIC X(15)  VALUE 'Order to purch.'.          
001728     03 XLS-RUB-TAB29        PIC X      VALUE X'05'.                      
001729     03 XLS-RUB-TIAVTAL      PIC X(16)  VALUE 'P/N Purch.Agree.'.         
001730     03 XLS-RUB-TAB30        PIC X      VALUE X'05'.                      
001731     03 XLS-RUB-KDLEVPST     PIC X(15)  VALUE 'Conf.Del.Sched.'.          
001732     03 XLS-RUB-TAB31        PIC X      VALUE X'05'.                      
001733     03 XLS-RUB-TILEVBSK     PIC X(21)                                    
001734                              VALUE 'Planned delivery date'.              
001735     03 XLS-RUB-TAB32        PIC X      VALUE X'05'.                      
001736     03 XLS-RUB-KDMDS        PIC X(4)   VALUE 'IMDS'.                     
001737     03 XLS-RUB-TAB33        PIC X      VALUE X'05'.                      
001738     03 XLS-RUB-VKART-KDP    PIC X(6)   VALUE 'Weight'.                   
001739     03 XLS-RUB-TAB34        PIC X      VALUE X'05'.                      
001740     03 XLS-RUB-TITPD-AAVV   PIC X(8)   VALUE 'TPD week'.                 
001741     03 XLS-RUB-TAB35        PIC X      VALUE X'05'.                      
001742     03 XLS-RUB-KDTPD        PIC X(10)  VALUE 'TPD status'.               
001743     03 XLS-RUB-TAB36        PIC X      VALUE X'05'.                      
001744     03 XLS-RUB-DAPSWQP-1    PIC X(14)  VALUE 'Plan PSW1 week'.           
001745     03 XLS-RUB-TAB37        PIC X      VALUE X'05'.                      
001746     03 XLS-RUB-KDPSWQP-1    PIC X(16)  VALUE 'Plan PSW1 status'.         
001747     03 XLS-RUB-TAB38        PIC X      VALUE X'05'.                      
001748     03 XLS-RUB-DAPSWPP-2    PIC X(14)  VALUE 'Plan PSW2 week'.           
001749     03 XLS-RUB-TAB39        PIC X      VALUE X'05'.                      
001750     03 XLS-RUB-KDPSWPP-2    PIC X(16)  VALUE 'Plan PSW2 status'.         
001751     03 XLS-RUB-TAB40        PIC X      VALUE X'05'.                      
001752     03 XLS-RUB-DAPSWCP-3    PIC X(14)  VALUE 'Plan PSW3 week'.           
001753     03 XLS-RUB-TAB41        PIC X      VALUE X'05'.                      
001754     03 XLS-RUB-KDPSWCP-3    PIC X(16)  VALUE 'Plan PSW3 status'.         
001755     03 XLS-RUB-TAB42        PIC X      VALUE X'05'.                      
001756     03 XLS-RUB-DAPSWQA-1    PIC X(13)  VALUE 'Act PSW1 week'.            
001757     03 XLS-RUB-TAB43        PIC X      VALUE X'05'.                      
001758     03 XLS-RUB-KDPSWQA-1    PIC X(15)  VALUE 'Act PSW1 status'.          
001759     03 XLS-RUB-TAB44        PIC X      VALUE X'05'.                      
001760     03 XLS-RUB-DAPSWPA-2    PIC X(13)  VALUE 'Act PSW2 week'.            
001761     03 XLS-RUB-TAB45        PIC X      VALUE X'05'.                      
001762     03 XLS-RUB-KDPSWPA-2    PIC X(15)  VALUE 'Act PSW2 status'.          
001763     03 XLS-RUB-TAB46        PIC X      VALUE X'05'.                      
001764     03 XLS-RUB-DAPSWCA-3    PIC X(13)  VALUE 'Act PSW3 week'.            
001765     03 XLS-RUB-TAB47        PIC X      VALUE X'05'.                      
001766     03 XLS-RUB-KDPSWCA-3    PIC X(15)  VALUE 'Act PSW3 status'.          
001767     03 XLS-RUB-TAB48        PIC X      VALUE X'05'.                      
001768     03 XLS-RUB-FLPULSPR     PIC X(10)  VALUE 'PULS price'.               
001770     03 XLS-RUB-TAB49        PIC X      VALUE X'05'.                      
001780     03 XLS-RUB-TIAVIDAT     PIC X(20)                                    
001781                              VALUE 'Last delivery to CDC'.               
001790     03 XLS-RUB-TAB50        PIC X      VALUE X'05'.                      
001800     03 XLS-RUB-SULEVANT     PIC X(15)  VALUE 'Parts delivered'.          
001900     03 XLS-RUB-TAB51        PIC X      VALUE X'05'.                      
001910     03 XLS-RUB-KVLS         PIC X(13)  VALUE 'Qty. in stock'.            
001920     03 XLS-RUB-TAB52        PIC X      VALUE X'05'.                      
001930     03 XLS-RUB-IDINK        PIC X(9)   VALUE 'Purchaser'.                
001940     03 XLS-RUB-TAB53        PIC X      VALUE X'05'.                      
001950     03 XLS-RUB-IDANSK       PIC X(8)   VALUE 'Procurer'.                 
001960     03 XLS-RUB-TAB54        PIC X      VALUE X'05'.                      
001970     03 XLS-RUB-IDSTEKN      PIC X(11)  VALUE 'Site techn.'.              
001980     03 XLS-RUB-TAB55        PIC X      VALUE X'05'.                      
001990     03 XLS-RUB-IDLEVNR      PIC X(8)   VALUE 'Supplier'.                 
001991     03 XLS-RUB-TAB56        PIC X      VALUE X'05'.                      
002000     03 XLS-RUB-IDPRODGR     PIC X(9)   VALUE 'Prod. Mgr'.                
002100     03 XLS-RUB-TAB57        PIC X      VALUE X'05'.                      
008200     03 XLS-RUB-KVYVOL-INT   PIC X(15)  VALUE 'Dec.Launch Vol.'.          
008210     03 XLS-RUB-TAB58        PIC X      VALUE X'05'.                      
008400     03 XLS-RUB-KVYVOL-B3    PIC X(14)  VALUE 'Volume Board 3'.           
008410     03 XLS-RUB-TAB59        PIC X      VALUE X'05'.                      
008411     03 XLS-RUB-KVYVOL-B2    PIC X(14)  VALUE 'Volume Board 2'.           
008420     03 XLS-RUB-TAB60        PIC X      VALUE X'05'.                      
008421     03 XLS-RUB-KVYVOL-B1    PIC X(14)  VALUE 'Volume Board 1'.           
008430     03 XLS-RUB-TAB61        PIC X      VALUE X'05'.                      
008431     03 XLS-RUB-KVYVOL-ASS   PIC X(13)  VALUE 'Ass.Year Vol.'.            
008440     03 XLS-RUB-TAB62        PIC X      VALUE X'05'.                      
008441     03 XLS-RUB-BEMAPP       PIC X(6)   VALUE 'Binder'.                   
008450     03 XLS-RUB-TAB63        PIC X      VALUE X'05'.                      
008451     03 XLS-RUB-KVFOTO       PIC X(14)  VALUE 'Photo mtrl Qty'.           
008460     03 XLS-RUB-TAB64        PIC X      VALUE X'05'.                      
008470     03 XLS-RUB-TIFOTO       PIC X(15)  VALUE 'Photo mtrl Week'.          
008500     03 XLS-RUB-TAB65        PIC X      VALUE X'05'.                      
008600     03 XLS-RUB-TEVERKTYG    PIC X(13)  VALUE 'Tool Purchase'.            
008700     03 XLS-RUB-TAB66        PIC X      VALUE X'05'.                      
008800     03 XLS-RUB-TESTATXT     PIC X(11)  VALUE 'STA comment'.              
008900     03 XLS-RUB-TAB67        PIC X      VALUE X'05'.                      
009000     03 XLS-RUB-TEMATXT      PIC X(10)  VALUE 'MA comment'.               
009100     03 XLS-RUB-TAB68        PIC X      VALUE X'05'.                      
009200     03 XLS-RUB-TEINKTXT     PIC X(11)  VALUE 'INK comment'.              
009300     03 XLS-RUB-TAB69        PIC X      VALUE X'05'.                      
009400     03 XLS-RUB-TEANSTXT     PIC X(12)  VALUE 'ANSK comment'.             
009500     03 XLS-RUB-TAB70        PIC X      VALUE X'05'.                      
009600     03 XLS-RUB-TEAUXTXT     PIC X(11)  VALUE 'AUX comment'.              
009700     03 XLS-RUB-TAB71        PIC X      VALUE X'05'.                      
009800                                                                          
