//WDT1RLV9 JOB (650W0020200WDT1RLV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDT1CLU                                          
//*                                                                             
//WDT1    EXEC WG01REL,                                                         
//             DBD=WDT1,COND.ABEND=(4,GE,REL)                                   
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(4096,(1800,180),RLSE)                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDT1K(+0),DISP=SHR                              
//REL.WDT1K DD DSN=WG01.QASE.WDT1K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDT1K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDT1ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDT1BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDT1CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDT1DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDT1ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDT1FCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDT1GCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDT1HCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDT1ICLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDT1AK DD DSN=WG01.QASE.WDT1AK,DISP=SHR                                   
//SIU.WDT1BK DD DSN=WG01.QASE.WDT1BK,DISP=SHR                                   
//SIU.WDT1CK DD DSN=WG01.QASE.WDT1CK,DISP=SHR                                   
//SIU.WDT1DK DD DSN=WG01.QASE.WDT1DK,DISP=SHR                                   
//SIU.WDT1EK DD DSN=WG01.QASE.WDT1EK,DISP=SHR                                   
//SIU.WDT1FK DD DSN=WG01.QASE.WDT1FK,DISP=SHR                                   
//SIU.WDT1GK DD DSN=WG01.QASE.WDT1GK,DISP=SHR                                   
//SIU.WDT1HK DD DSN=WG01.QASE.WDT1HK,DISP=SHR                                   
//SIU.WDT1IK DD DSN=WG01.QASE.WDT1IK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:50                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDT1,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDT1RLV9                                         
