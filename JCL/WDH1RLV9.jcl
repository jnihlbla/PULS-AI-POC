//WDH1RLV9 JOB (640W0020200WDH1RLV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS EXEC PGM=IDCAMS                                                        
//SYSPRINT DD SYSOUT=*                                                          
  DELETE (WG01.QASE.WDH1V) NONVSAM PURGE                                        
//*                                                                             
//ALLOC    EXEC PGM=IEFBR14                                                     
//WDH1V  DD DSN=WG01.QASE.WDH1V,                                                
//           DISP=(NEW,CATLG,DELETE),                                           
//           SPACE=(2048,(1980,630),,CONTIG,ROUND),                             
//           MGMTCLAS=TP0,DATACLAS=MVOL                                         
//*                                                                             
//WDH1    EXEC WG01REL,                                                         
//             DBD=WDH1                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             DCB=BUFNO=10,                                                    
//             SPACE=(2048,(1980,900),RLSE)                                     
//REL.DBORELD1 DD DSN=WG01.UNLO.WDH1V(+0),DISP=SHR                              
//REL.WDH1V DD DSN=WG01.QASE.WDH1V,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDH1V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDH1ACLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDH1BCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDH1CCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDH1DCLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDH1ECLU),DISP=SHR                            
//         DD DSN=W.QASE.CONSTANT(WDH1FCLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDH1AK DD DSN=WG01.QASE.WDH1AK,DISP=SHR                                   
//SIU.WDH1BK DD DSN=WG01.QASE.WDH1BK,DISP=SHR                                   
//SIU.WDH1CK DD DSN=WG01.QASE.WDH1CK,DISP=SHR                                   
//SIU.WDH1DK DD DSN=WG01.QASE.WDH1DK,DISP=SHR                                   
//SIU.WDH1EK DD DSN=WG01.QASE.WDH1EK,DISP=SHR                                   
//SIU.WDH1FK DD DSN=WG01.QASE.WDH1FK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:43                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDH1,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDH1RLV9                                         
