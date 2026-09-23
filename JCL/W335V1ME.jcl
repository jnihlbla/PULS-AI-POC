//W335V1ME JOB (650W3350100W335V1ME,W100),'RTN W335V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=M                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
/*CNTL V335,EXC                                                                 
//*                                                                             
//******** MEMO MARKNADSFÖRING                                                  
//W335MFXX EXEC WEMPTST,DSIN=W335.W335V1.W33511(+0)                             
//         EXEC WMEMOSND,CONDS='(0,LT,W335MFXX.T)',                             
//             DSIN=W335.W335V1.W33511(+0),                                     
//             REQS=W33511ME                                                    
//*                                                                             
//******** MEMO MARKNADSBOLAG A SVERIGE                                         
//W335MBAX EXEC WEMPTST,DSIN=W335.W335V1.W33512(+0)                             
//         EXEC WMEMOSND,CONDS='(0,LT,W335MBAX.T)',                             
//             DSIN=W335.W335V1.W33512(+0),                                     
//             REQS=W33512ME                                                    
//*                                                                             
//******** MEMO MARKNADSBOLAG B VCEM EUROPA                                     
//W335MBBX EXEC WEMPTST,DSIN=W335.W335V1.W33513(+0)                             
//         EXEC WMEMOSND,CONDS='(0,LT,W335MBBX.T)',                             
//             DSIN=W335.W335V1.W33513(+0),                                     
//             REQS=W33513ME                                                    
//*                                                                             
//******** MEMO MARKNADSBOLAG C ASIA PACIFIC                                    
//W335MBCX EXEC WEMPTST,DSIN=W335.W335V1.W33514(+0)                             
//         EXEC WMEMOSND,CONDS='(0,LT,W335MBCX.T)',                             
//             DSIN=W335.W335V1.W33514(+0),                                     
//             REQS=W33514ME                                                    
//*                                                                             
//******** MEMO MARKNADSBOLAG D VCSA SOUTH AMERICA                              
//W335MBDX EXEC WEMPTST,DSIN=W335.W335V1.W33515(+0)                             
//         EXEC WMEMOSND,CONDS='(0,LT,W335MBDX.T)',                             
//             DSIN=W335.W335V1.W33515(+0),                                     
//             REQS=W33515ME                                                    
//*                                                                             
//******** MEMO MARKNADSBOLAG E VCNA                                            
//W335MBEX EXEC WEMPTST,DSIN=W335.W335V1.W33516(+0)                             
//         EXEC WMEMOSND,CONDS='(0,LT,W335MBEX.T)',                             
//             DSIN=W335.W335V1.W33516(+0),                                     
//             REQS=W33516ME                                                    
//*                                                                             
//******** MEMO MARKNADSBOLAG F VCAS                                            
//W335MBFX EXEC WEMPTST,DSIN=W335.W335V1.W33517(+0)                             
//         EXEC WMEMOSND,CONDS='(0,LT,W335MBFX.T)',                             
//             DSIN=W335.W335V1.W33517(+0),                                     
//             REQS=W33517ME                                                    
//*                                                                             
//******** MEMO MARKNADSBOLAG G VCI                                             
//W335MBGX EXEC WEMPTST,DSIN=W335.W335V1.W33518(+0)                             
//         EXEC WMEMOSND,CONDS='(0,LT,W335MBGX.T)',                             
//             DSIN=W335.W335V1.W33518(+0),                                     
//             REQS=W33518ME                                                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W335V1ME                                         
