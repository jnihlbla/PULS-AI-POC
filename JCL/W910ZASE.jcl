//W910ZASE JOB (640W9100100W910ZASE,W100),'RTN W910P2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM LINES=99,FORMS=1800,LINECT=0                                          
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//TOMJ1    EXEC WEMPTST,DSIN=W910.W910P2.W910J1(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ1.T),                                    
//             DSIN=W910.W910P2.W910J1(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMJ2    EXEC WEMPTST,DSIN=W910.W910P2.W910J2(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ2.T),                                    
//             DSIN=W910.W910P2.W910J2(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMJ3    EXEC WEMPTST,DSIN=W910.W910P2.W910J3(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ3.T),                                    
//             DSIN=W910.W910P2.W910J3(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMJ4    EXEC WEMPTST,DSIN=W910.W910P2.W910J4(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ4.T),                                    
//             DSIN=W910.W910P2.W910J4(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMJ5    EXEC WEMPTST,DSIN=W910.W910P2.W910J5(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ5.T),                                    
//             DSIN=W910.W910P2.W910J5(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMJ6    EXEC WEMPTST,DSIN=W910.W910P2.W910J6(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ6.T),                                    
//             DSIN=W910.W910P2.W910J6(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMJ7    EXEC WEMPTST,DSIN=W910.W910P2.W910J7(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ7.T),                                    
//             DSIN=W910.W910P2.W910J7(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMJ8    EXEC WEMPTST,DSIN=W910.W910P2.W910J8(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ8.T),                                    
//             DSIN=W910.W910P2.W910J8(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMJ9    EXEC WEMPTST,DSIN=W910.W910P2.W910J9(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMJ9.T),                                    
//             DSIN=W910.W910P2.W910J9(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*---------------------------------------------------------                    
//TOMK1    EXEC WEMPTST,DSIN=W910.W910P2.W910K1(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK1.T),                                    
//             DSIN=W910.W910P2.W910K1(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMK2    EXEC WEMPTST,DSIN=W910.W910P2.W910K2(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK2.T),                                    
//             DSIN=W910.W910P2.W910K2(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMK3    EXEC WEMPTST,DSIN=W910.W910P2.W910K3(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK3.T),                                    
//             DSIN=W910.W910P2.W910K3(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMK4    EXEC WEMPTST,DSIN=W910.W910P2.W910K4(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK4.T),                                    
//             DSIN=W910.W910P2.W910K4(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMK5    EXEC WEMPTST,DSIN=W910.W910P2.W910K5(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK5.T),                                    
//             DSIN=W910.W910P2.W910K5(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMK6    EXEC WEMPTST,DSIN=W910.W910P2.W910K6(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK6.T),                                    
//             DSIN=W910.W910P2.W910K6(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMK7    EXEC WEMPTST,DSIN=W910.W910P2.W910K7(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK7.T),                                    
//             DSIN=W910.W910P2.W910K7(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMK8    EXEC WEMPTST,DSIN=W910.W910P2.W910K8(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK8.T),                                    
//             DSIN=W910.W910P2.W910K8(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMK9    EXEC WEMPTST,DSIN=W910.W910P2.W910K9(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMK9.T),                                    
//             DSIN=W910.W910P2.W910K9(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//*---------------------------------------------------------                    
//TOML1    EXEC WEMPTST,DSIN=W910.W910P2.W910L1(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML1.T),                                    
//             DSIN=W910.W910P2.W910L1(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOML2    EXEC WEMPTST,DSIN=W910.W910P2.W910L2(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML2.T),                                    
//             DSIN=W910.W910P2.W910L2(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOML3    EXEC WEMPTST,DSIN=W910.W910P2.W910L3(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML3.T),                                    
//             DSIN=W910.W910P2.W910L3(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOML4    EXEC WEMPTST,DSIN=W910.W910P2.W910L4(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML4.T),                                    
//             DSIN=W910.W910P2.W910L4(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOML5    EXEC WEMPTST,DSIN=W910.W910P2.W910L5(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML5.T),                                    
//             DSIN=W910.W910P2.W910L5(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOML6    EXEC WEMPTST,DSIN=W910.W910P2.W910L6(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML6.T),                                    
//             DSIN=W910.W910P2.W910L6(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOML7    EXEC WEMPTST,DSIN=W910.W910P2.W910L7(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML7.T),                                    
//             DSIN=W910.W910P2.W910L7(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOML8    EXEC WEMPTST,DSIN=W910.W910P2.W910L8(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML8.T),                                    
//             DSIN=W910.W910P2.W910L8(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOML9    EXEC WEMPTST,DSIN=W910.W910P2.W910L9(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOML9.T),                                    
//             DSIN=W910.W910P2.W910L9(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*---------------------------------------------------------                    
//TOMM1    EXEC WEMPTST,DSIN=W910.W910P2.W910M1(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM1.T),                                    
//             DSIN=W910.W910P2.W910M1(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMM2    EXEC WEMPTST,DSIN=W910.W910P2.W910M2(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM2.T),                                    
//             DSIN=W910.W910P2.W910M2(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMM3    EXEC WEMPTST,DSIN=W910.W910P2.W910M3(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM3.T),                                    
//             DSIN=W910.W910P2.W910M3(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMM4    EXEC WEMPTST,DSIN=W910.W910P2.W910M4(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM4.T),                                    
//             DSIN=W910.W910P2.W910M4(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMM5    EXEC WEMPTST,DSIN=W910.W910P2.W910M5(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM5.T),                                    
//             DSIN=W910.W910P2.W910M5(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMM6    EXEC WEMPTST,DSIN=W910.W910P2.W910M6(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM6.T),                                    
//             DSIN=W910.W910P2.W910M6(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMM7    EXEC WEMPTST,DSIN=W910.W910P2.W910M7(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM7.T),                                    
//             DSIN=W910.W910P2.W910M7(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMM8    EXEC WEMPTST,DSIN=W910.W910P2.W910M8(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM8.T),                                    
//             DSIN=W910.W910P2.W910M8(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMM9    EXEC WEMPTST,DSIN=W910.W910P2.W910M9(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMM9.T),                                    
//             DSIN=W910.W910P2.W910M9(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*---------------------------------------------------------                    
//TOMN1    EXEC WEMPTST,DSIN=W910.W910P2.W910N1(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN1.T),                                    
//             DSIN=W910.W910P2.W910N1(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMN2    EXEC WEMPTST,DSIN=W910.W910P2.W910N2(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN2.T),                                    
//             DSIN=W910.W910P2.W910N2(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMN3    EXEC WEMPTST,DSIN=W910.W910P2.W910N3(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN3.T),                                    
//             DSIN=W910.W910P2.W910N3(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMN4    EXEC WEMPTST,DSIN=W910.W910P2.W910N4(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN4.T),                                    
//             DSIN=W910.W910P2.W910N4(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMN5    EXEC WEMPTST,DSIN=W910.W910P2.W910N5(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN5.T),                                    
//             DSIN=W910.W910P2.W910N5(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMN6    EXEC WEMPTST,DSIN=W910.W910P2.W910N6(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN6.T),                                    
//             DSIN=W910.W910P2.W910N6(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMN7    EXEC WEMPTST,DSIN=W910.W910P2.W910N7(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN7.T),                                    
//             DSIN=W910.W910P2.W910N7(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMN8    EXEC WEMPTST,DSIN=W910.W910P2.W910N8(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN8.T),                                    
//             DSIN=W910.W910P2.W910N8(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMN9    EXEC WEMPTST,DSIN=W910.W910P2.W910N9(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMN9.T),                                    
//             DSIN=W910.W910P2.W910N9(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//*---------------------------------------------------------                    
//TOMO1    EXEC WEMPTST,DSIN=W910.W910P2.W910O1(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO1.T),                                    
//             DSIN=W910.W910P2.W910O1(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMO2    EXEC WEMPTST,DSIN=W910.W910P2.W910O2(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO2.T),                                    
//             DSIN=W910.W910P2.W910O2(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMO3    EXEC WEMPTST,DSIN=W910.W910P2.W910O3(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO3.T),                                    
//             DSIN=W910.W910P2.W910O3(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMO4    EXEC WEMPTST,DSIN=W910.W910P2.W910O4(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO4.T),                                    
//             DSIN=W910.W910P2.W910O4(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMO5    EXEC WEMPTST,DSIN=W910.W910P2.W910O5(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO5.T),                                    
//             DSIN=W910.W910P2.W910O5(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMO6    EXEC WEMPTST,DSIN=W910.W910P2.W910O6(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO6.T),                                    
//             DSIN=W910.W910P2.W910O6(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMO7    EXEC WEMPTST,DSIN=W910.W910P2.W910O7(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO7.T),                                    
//             DSIN=W910.W910P2.W910O7(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMO8    EXEC WEMPTST,DSIN=W910.W910P2.W910O8(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO8.T),                                    
//             DSIN=W910.W910P2.W910O8(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMO9    EXEC WEMPTST,DSIN=W910.W910P2.W910O9(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMO9.T),                                    
//             DSIN=W910.W910P2.W910O9(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//*---------------------------------------------------------                    
//TOMP1    EXEC WEMPTST,DSIN=W910.W910P2.W910P1(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP1.T),                                    
//             DSIN=W910.W910P2.W910P1(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMP2    EXEC WEMPTST,DSIN=W910.W910P2.W910P2(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP2.T),                                    
//             DSIN=W910.W910P2.W910P2(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMP3    EXEC WEMPTST,DSIN=W910.W910P2.W910P3(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP3.T),                                    
//             DSIN=W910.W910P2.W910P3(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMP4    EXEC WEMPTST,DSIN=W910.W910P2.W910P4(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP4.T),                                    
//             DSIN=W910.W910P2.W910P4(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMP5    EXEC WEMPTST,DSIN=W910.W910P2.W910P5(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP5.T),                                    
//             DSIN=W910.W910P2.W910P5(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMP6    EXEC WEMPTST,DSIN=W910.W910P2.W910P6(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP6.T),                                    
//             DSIN=W910.W910P2.W910P6(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMP7    EXEC WEMPTST,DSIN=W910.W910P2.W910P7(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP7.T),                                    
//             DSIN=W910.W910P2.W910P7(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMP8    EXEC WEMPTST,DSIN=W910.W910P2.W910P8(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP8.T),                                    
//             DSIN=W910.W910P2.W910P8(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMP9    EXEC WEMPTST,DSIN=W910.W910P2.W910P9(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMP9.T),                                    
//             DSIN=W910.W910P2.W910P9(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//*---------------------------------------------------------                    
//TOMQ1    EXEC WEMPTST,DSIN=W910.W910P2.W910Q1(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMQ1.T),                                    
//             DSIN=W910.W910P2.W910Q1(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMQ2    EXEC WEMPTST,DSIN=W910.W910P2.W910Q2(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMQ2.T),                                    
//             DSIN=W910.W910P2.W910Q2(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMQ3    EXEC WEMPTST,DSIN=W910.W910P2.W910Q3(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMQ3.T),                                    
//             DSIN=W910.W910P2.W910Q3(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMQ4    EXEC WEMPTST,DSIN=W910.W910P2.W910Q4(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMQ4.T),                                    
//             DSIN=W910.W910P2.W910Q4(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//TOMQ5    EXEC WEMPTST,DSIN=W910.W910P2.W910Q5(+0)                             
//WQSEN   EXEC WZ11P023,COND=(0,LT,TOMQ5.T),                                    
//             DSIN=W910.W910P2.W910Q5(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTDESCRIPTIONS                                       
/*                                                                              
//*                                                                             
//*---------------------------------------------------------                    
//SOPEND  EXEC WSOPEND,PROCESS=W910ZASE                                         
